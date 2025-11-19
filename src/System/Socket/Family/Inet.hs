{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      :  System.Socket.Family.Inet
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
module System.Socket.Family.Inet (
  -- * Inet
  Inet,

  -- ** InetAddress
  InetAddress,

  -- ** InetPort
  InetPort,
  SocketAddress (SocketAddressInet, inetAddress, inetPort),

  -- * Custom addresses

  -- ** inetAddressFromTuple
  inetAddressFromTuple,

  -- ** inetAddressToTuple
  inetAddressToTuple,

  -- * Special addresses

  -- ** inetAllHostsGroup
  inetAllHostsGroup,

  -- ** inetAny
  inetAny,

  -- ** inetBroadcast
  inetBroadcast,

  -- ** inetLoopback
  inetLoopback,

  -- ** inetMaxLocalGroup
  inetMaxLocalGroup,

  -- ** inetNone
  inetNone,

  -- ** inetUnspecificGroup
  inetUnspecificGroup,
) where

import Control.Monad (when)
import Data.List
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import System.Socket.Internal.Constants
import System.Socket.Internal.Platform
import System.Socket.Internal.Socket

-- | The [Internet Protocol version 4](https://en.wikipedia.org/wiki/IPv4).
data Inet

instance Family Inet where
  familyNumber _ = c_AF_INET

  -- \| An [IPv4](https://en.wikipedia.org/wiki/IPv4) socket address.
  --
  --   The socket address contains a port number that may be used by transport
  --   protocols like [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol).
  --
  -- > SocketAddressInet inetLoopback 8080
  data SocketAddress Inet
    = SocketAddressInet
    { inetAddress :: InetAddress
    , inetPort :: InetPort
    }
    deriving (Eq, Show)

-- | To avoid errors with endianess it was decided to keep this type abstract.
--
--   Use `inetAddressFromTuple` and `inetAddressToTuple` for constructing and
--   deconstructing custom addresses.
--
--   Hint: Use the `Foreign.Storable.Storable` instance.
--   It exposes it exactly as found within an IP packet (big endian if you insist
--   on interpreting it as a number).
--
--   Another hint: Use `System.Socket.getAddressInfo` for parsing and suppress
--   nameserver lookups:
--
--   > > getAddressInfo (Just "127.0.0.1") Nothing aiNumericHost :: IO [AddressInfo Inet Stream TCP]
--   > [AddressInfo {addressInfoFlags = AddressInfoFlags 4, socketAddress = SocketAddressInet {inetAddress = InetAddress 127.0.0.1, inetPort = InetPort 0}, canonicalName = Nothing}]
newtype InetAddress
  = InetAddress Word32
  deriving (Eq)

newtype InetPort = InetPort Word16
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

-- | Constructs a custom `InetAddress`.
--
--   > inetAddressFromTuple (127,0,0,1) == inetLoopback
inetAddressFromTuple :: (Word8, Word8, Word8, Word8) -> InetAddress
inetAddressFromTuple (w0, w1, w2, w3) =
  InetAddress $ foldl1' (\x y -> x * 256 + y) [f w0, f w1, f w2, f w3]
 where
  f = fromIntegral

-- | Deconstructs an `InetAddress`.
inetAddressToTuple :: InetAddress -> (Word8, Word8, Word8, Word8)
inetAddressToTuple (InetAddress a) =
  (w0, w1, w2, w3)
 where
  w0 = fromIntegral $ rem (quot a $ 256 * 256 * 256) 256
  w1 = fromIntegral $ rem (quot a $ 256 * 256) 256
  w2 = fromIntegral $ rem (quot a 256) 256
  w3 = fromIntegral $ rem a 256

-- | @0.0.0.0@
inetAny :: InetAddress
inetAny = InetAddress 0

-- | @255.255.255.255@
inetBroadcast :: InetAddress
inetBroadcast = InetAddress $ foldl1' (\x y -> x * 256 + y) [255, 255, 255, 255]

-- | @255.255.255.255@
inetNone :: InetAddress
inetNone = InetAddress $ foldl1' (\x y -> x * 256 + y) [255, 255, 255, 255]

-- | @127.0.0.1@
inetLoopback :: InetAddress
inetLoopback = InetAddress $ foldl1' (\x y -> x * 256 + y) [127, 0, 0, 1]

-- | @224.0.0.0@
inetUnspecificGroup :: InetAddress
inetUnspecificGroup = InetAddress $ foldl1' (\x y -> x * 256 + y) [224, 0, 0, 0]

-- | @224.0.0.1@
inetAllHostsGroup :: InetAddress
inetAllHostsGroup = InetAddress $ foldl1' (\x y -> x * 256 + y) [224, 0, 0, 1]

-- | @224.0.0.255@
inetMaxLocalGroup :: InetAddress
inetMaxLocalGroup = InetAddress $ foldl1' (\x y -> x * 256 + y) [224, 0, 0, 255]

instance Show InetAddress where
  show (InetAddress a) =
    ("InetAddress " ++) $
      intercalate "." $
        map (\p -> show $ a `div` 256 ^ p `mod` 256) [3, 2, 1, 0 :: Word32]

instance Storable InetPort where
  sizeOf _ = sizeOf (undefined :: Word16)
  alignment _ = alignment (undefined :: Word16)
  peek ptr = do
    p0 <- peekByteOff ptr 0 :: IO Word8
    p1 <- peekByteOff ptr 1 :: IO Word8
    return $ InetPort (fromIntegral p0 * 256 + fromIntegral p1)
  poke ptr (InetPort w16) = do
    pokeByteOff ptr 0 (w16_0 w16)
    pokeByteOff ptr 1 (w16_1 w16)
   where
    w16_0, w16_1 :: Word16 -> Word8
    w16_0 x = fromIntegral $ rem (quot x 256) 256
    w16_1 x = fromIntegral $ rem x 256

sockaddrInSize :: Int
sockaddrInSize = fromIntegral c_sizeof_sockaddr_in

sockaddrInAlignment :: Int
sockaddrInAlignment = fromIntegral c_alignof_sockaddr_in

sockaddrInSinFamilyOffset
  , sockaddrInSinPortOffset
  , sockaddrInSinAddrOffset
    :: Int
sockaddrInSinFamilyOffset = fromIntegral c_offset_sockaddr_in_sin_family
sockaddrInSinPortOffset = fromIntegral c_offset_sockaddr_in_sin_port
sockaddrInSinAddrOffset = fromIntegral c_offset_sockaddr_in_sin_addr

sockaddrInHasLen :: Bool
sockaddrInHasLen = c_has_sockaddr_in_len /= 0

sockaddrInSinLenOffset :: Int
sockaddrInSinLenOffset = fromIntegral c_offset_sockaddr_in_sin_len

inAddrSAddrOffset :: Int
inAddrSAddrOffset = fromIntegral c_offset_in_addr_s_addr

sinPortPtr :: Ptr (SocketAddress Inet) -> Ptr InetPort
sinPortPtr = (`plusPtr` sockaddrInSinPortOffset) . castPtr

sinAddrPtr :: Ptr (SocketAddress Inet) -> Ptr InetAddress
sinAddrPtr = (`plusPtr` (sockaddrInSinAddrOffset + inAddrSAddrOffset)) . castPtr

sinLenPtr :: Ptr (SocketAddress Inet) -> Ptr Word8
sinLenPtr = (`plusPtr` sockaddrInSinLenOffset) . castPtr

saFamilySize :: Int
saFamilySize = fromIntegral c_sizeof_sa_family

pokeSaFamily :: Ptr (SocketAddress Inet) -> Word16 -> IO ()
pokeSaFamily ptr val =
  case saFamilySize of
    1 ->
      pokeByteOff
        ptr
        sockaddrInSinFamilyOffset
        (fromIntegral val :: Word8)
    2 ->
      pokeByteOff
        ptr
        sockaddrInSinFamilyOffset
        val
    _ -> error "Unsupported sa_family_t size for Inet sockets"

instance Storable InetAddress where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek ptr = do
    i0 <- peekByteOff ptr 0 :: IO Word8
    i1 <- peekByteOff ptr 1 :: IO Word8
    i2 <- peekByteOff ptr 2 :: IO Word8
    i3 <- peekByteOff ptr 3 :: IO Word8
    return $ InetAddress $ (((((f i0 * 256) + f i1) * 256) + f i2) * 256) + f i3
   where
    f = fromIntegral
  poke ptr (InetAddress a) = do
    pokeByteOff ptr 0 (fromIntegral $ rem (quot a $ 256 * 256 * 256) 256 :: Word8)
    pokeByteOff ptr 1 (fromIntegral $ rem (quot a $ 256 * 256) 256 :: Word8)
    pokeByteOff ptr 2 (fromIntegral $ rem (quot a 256) 256 :: Word8)
    pokeByteOff ptr 3 (fromIntegral $ rem a 256 :: Word8)

instance Storable (SocketAddress Inet) where
  sizeOf _ = sockaddrInSize
  alignment _ = sockaddrInAlignment
  peek ptr = do
    a <- peek (sinAddrPtr ptr)
    p <- peek (sinPortPtr ptr)
    return $ SocketAddressInet a p
  poke ptr (SocketAddressInet a p) = do
    c_memset ptr 0 (fromIntegral c_sizeof_sockaddr_in)
    when sockaddrInHasLen $
      poke (sinLenPtr ptr) (fromIntegral sockaddrInSize :: Word8)
    pokeSaFamily ptr (fromIntegral c_AF_INET)
    poke (sinAddrPtr ptr) a
    poke (sinPortPtr ptr) p
