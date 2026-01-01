--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      :  System.Socket.Type.Stream
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
module System.Socket.Type.Stream (
  -- * Stream
  Stream,

  -- ** Specialized send operations

  -- *** sendAll
  sendAll,

  -- *** sendAllLazy
  sendAllLazy,

  -- *** sendAllBuilder
  sendAllBuilder,
  sendAllBuilderWithBufSize,

  -- ** Specialized receive operations

  -- *** receiveAllLazy
  receiveAllLazy,
) where

import Control.Monad (when)
import Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Builder.Extra as BE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Unsafe as BS
import Data.Int
import Data.Monoid
import Data.Word
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr
import System.Socket
import System.Socket.Internal.Constants
import System.Socket.Unsafe

data Stream

instance Type Stream where
  typeNumber _ = c_SOCK_STREAM

-- | Sends a whole `BS.ByteString` with as many system calls as necessary
--   and returns the bytes sent (in this case just the `BS.ByteString`s
--   `BS.length`).
sendAll :: Socket f Stream p -> BS.ByteString -> MessageFlags -> IO Int
sendAll s bs flags = do
  BS.unsafeUseAsCStringLen bs (uncurry sendAllPtr)
  return (BS.length bs)
 where
  sendAllPtr :: Ptr a -> Int -> IO ()
  sendAllPtr ptr len = do
    sent <- fromIntegral `fmap` unsafeSend s ptr (fromIntegral len) flags
    when (sent < len) $ sendAllPtr (plusPtr ptr sent) (len - sent)

-- | Like `sendAll`, but operates on lazy `Data.ByteString.Lazy.ByteString`s.
--
--   It uses `sendAll` internally to send all chunks sequentially. The lock on
--   the socket is acquired for each chunk separately, so the socket can be read
--   from in an interleaving fashion.
sendAllLazy :: Socket f Stream p -> LBS.ByteString -> MessageFlags -> IO Int64
sendAllLazy s lbs flags =
  LBS.foldlChunks f (return 0) lbs
 where
  f action bs = do
    sent <- action
    sent' <- fromIntegral `fmap` sendAll s bs flags
    return $! sent + sent'

-- | Sends a whole `BB.Builder` without allocating `BS.ByteString`s.
--
-- Normally, this should be the fastest option for sending data in streaming
-- communication.
sendAllBuilder
  :: Socket f Stream p
  -- ^ Socket to send on
  -> BB.Builder
  -- ^ Message to send (the \"payload\")
  -> MessageFlags
  -- ^ @send@ message flags (the @flags@ argument of the underlying OS call)
  -> IO Int64
  -- ^ Total bytes sent
sendAllBuilder s builder flags = sendAllBuilderWithBufSize s 4096 builder flags

-- | Like `sendAllBuilder`, but allows configuring the \"initial buffer size\".
--
-- Buffer size explanation: When running a `Data.ByteString.Builder.Builder`,
-- either a reference to an existing strict `Data.ByteString.ByteString` is
-- returned, or a memory location is requested (the builder needs to write
-- bytes to it). We allocate a buffer for this, whose size is 4096 bytes by
-- default.
--
-- This buffer is reused repeatedly until the builder finishes running. When
-- the builder requests more space than the current buffer has, it's replaced
-- with a new buffer sized to the \"next power of two\" covering the request.
--
-- Therefore, if you know your builder will somehow request large contiguous
-- space, configuring the initial buffer size might reduce the number of
-- allocations and improve performance.
--
-- (Benchmark to see if a larger initial size actually helps; if you call this
-- frequently, repeatedly allocating\/freeing a larger buffer may in fact hurt
-- performance.)
sendAllBuilderWithBufSize
  :: Socket f Stream p
  -- ^ Socket to send on.
  -> Int
  -- ^ Initial buffer size in bytes. The caller is responsible for preventing a
  -- wrong value (e.g. a negative number)
  -> BB.Builder
  -- ^ Message to send (the \"payload\")
  -> MessageFlags
  -- ^ @send@ message flags (the @flags@ argument of the underlying OS call)
  -> IO Int64
  -- ^ Total bytes sent
sendAllBuilderWithBufSize s initialBufferSize builder flags = do
  let writer0 = BE.runBuilder builder
  fp0 <- mallocForeignPtrBytes initialBufferSize
  sendLoop writer0 fp0 initialBufferSize 0
 where
  sendLoop :: BE.BufferWriter -> ForeignPtr Word8 -> Int -> Int64 -> IO Int64
  sendLoop writer fp bufSize alreadySent = withForeignPtr fp $ \ptr -> do
    (written, next) <- writer ptr bufSize
    when (written > 0) $ sendAllPtr ptr written
    let sentSoFar = alreadySent + fromIntegral written
    case next of
      BE.Done -> return sentSoFar
      BE.Chunk bs writer' -> do
        bsSent <-
          if BS.null bs then return 0 else fromIntegral `fmap` sendAll s bs flags
        sendLoop writer' fp bufSize (sentSoFar + bsSent)
      BE.More minReq writer' -> do
        (newFp, newSize) <-
          if minReq <= bufSize
            then return (fp, bufSize)
            else do
              let newSize = nextPowerOfTwo minReq
              newFp <- mallocForeignPtrBytes newSize
              return (newFp, newSize)
        sendLoop writer' newFp newSize sentSoFar

  sendAllPtr :: Ptr Word8 -> Int -> IO ()
  sendAllPtr ptr len = do
    sent <- fromIntegral `fmap` unsafeSend s ptr (fromIntegral len) flags
    when (sent < len) $ sendAllPtr (plusPtr ptr sent) (len - sent)

  nextPowerOfTwo :: Int -> Int
  nextPowerOfTwo n
    | n <= 1 = 1
    | otherwise =
        let w = fromIntegral (n - 1) :: Word
            shiftAmount = finiteBitSize w - countLeadingZeros w
            resWord = (1 :: Word) `shiftL` shiftAmount
            capInt = bit (finiteBitSize (0 :: Int) - 1) :: Int
            resInt = fromIntegral resWord :: Int
         in if resInt <= 0 || resInt > capInt then capInt else resInt

-- | Like `receive`, but operates on lazy `Data.ByteString.Lazy.ByteString`s
-- and continues until either an empty part has been received (peer closed the
-- connection) or given buffer limit has been exceeded or an exception occured.
--
-- -  The `Data.Int.Int64` parameter is a soft limit on how many bytes to
--    receive. Collection is stopped if the limit has been exceeded. The result
--    might be up to one internal buffer size longer than the given limit. If
--    the returned `Data.ByteString.Lazy.ByteString`s length is lower than or
--    equal to the limit, the data has not been truncated and the transmission
--    is complete.
receiveAllLazy
  :: Socket f Stream p -> Int64 -> MessageFlags -> IO LBS.ByteString
receiveAllLazy sock maxLen flags = collect 0 Data.Monoid.mempty
 where
  collect len accum
    | len > maxLen = build accum
    | otherwise = do
        bs <- receive sock BE.smallChunkSize flags
        if BS.null bs
          then build accum
          else
            collect (len + fromIntegral (BS.length bs)) $!
              (accum `Data.Monoid.mappend` BB.byteString bs)
  build accum = return (BB.toLazyByteString accum)
