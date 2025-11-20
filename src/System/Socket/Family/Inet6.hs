{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      :  System.Socket.Family.Inet6
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
module System.Socket.Family.Inet6 (
  -- * Inet6
  Inet6,

  -- ** Inet6Address
  Inet6Address,

  -- ** Inet6Port
  Inet6Port,

  -- ** Inet6FlowInfo
  Inet6FlowInfo,

  -- ** Inet6ScopeId
  Inet6ScopeId,
  SocketAddress (
    SocketAddressInet6,
    inet6Address,
    inet6Port,
    inet6FlowInfo,
    inet6ScopeId
  ),

  -- * Custom addresses

  -- ** inet6AddressFromTuple
  inet6AddressFromTuple,

  -- ** inet6AddressToTuple
  inet6AddressToTuple,

  -- * Special addresses

  -- ** inet6Any
  inet6Any,

  -- ** inet6Loopback
  inet6Loopback,

  -- * Socket options

  -- ** V6Only
  V6Only (..),
) where

import Control.Applicative as A
import Control.Monad
import Data.Bits (unsafeShiftL, unsafeShiftR, (.|.))
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Word
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import System.Socket.Internal.Constants
import System.Socket.Internal.Endian
import System.Socket.Internal.Platform
import System.Socket.Internal.Socket
import System.Socket.Internal.SocketOption

-- | The [Internet Protocol version 6](https://en.wikipedia.org/wiki/IPv6).
data Inet6

instance Family Inet6 where
  familyNumber _ = c_AF_INET6

  -- \| An [IPv6](https://en.wikipedia.org/wiki/IPv6) socket address.
  --
  --   The socket address contains a port number that may be used by transport
  --   protocols like [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol).
  --
  -- > SocketAddressInet6 inet6Loopback 8080 0 0
  data SocketAddress Inet6
    = SocketAddressInet6
    { inet6Address :: Inet6Address
    , inet6Port :: Inet6Port
    , inet6FlowInfo :: Inet6FlowInfo
    , inet6ScopeId :: Inet6ScopeId
    }
    deriving (Eq, Show)

-- | To avoid errors with endianess it was decided to keep this type abstract.
--
--   Use `inet6AddressFromTuple` and `inet6AddressToTuple` for constructing and
--   deconstructing custom addresses.
--
--   Hint: Use the `Foreign.Storable.Storable` instance. It exposes it
--   exactly as found within an IP packet (big endian if you insist
--   on interpreting it as a number).
--
--   Another hint: Use `System.Socket.getAddressInfo` for parsing and suppress
--   nameserver lookups:
--
--   > > getAddressInfo (Just "::1") Nothing aiNumericHost :: IO [AddressInfo SocketAddressInet6 Stream TCP]
--   > [AddressInfo {
--   >    addressInfoFlags = AddressInfoFlags 4,
--   >    socketAddress    = SocketAddressInet6 {inet6Address = Inet6Address 0000:0000:0000:0000:0000:0000:0000:0001, inet6Port = Inet6Port 0, inet6FlowInfo = Inet6FlowInfo 0, inet6ScopeId = Inet6ScopeId 0},
--   >    canonicalName    = Nothing }]
data Inet6Address = Inet6Address {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Eq)

newtype Inet6Port = Inet6Port Word16
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype Inet6FlowInfo = Inet6FlowInfo Word32
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

newtype Inet6ScopeId = Inet6ScopeId Word32
  deriving (Eq, Ord, Show, Num, Real, Enum, Integral)

-- | Deconstructs an `Inet6Address`.
inet6AddressToTuple
  :: Inet6Address
  -> (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
inet6AddressToTuple (Inet6Address rawHb rawLb) =
  ( fromIntegral (hb `unsafeShiftR` 48)
  , fromIntegral (hb `unsafeShiftR` 32)
  , fromIntegral (hb `unsafeShiftR` 16)
  , fromIntegral hb
  , fromIntegral (lb `unsafeShiftR` 48)
  , fromIntegral (lb `unsafeShiftR` 32)
  , fromIntegral (lb `unsafeShiftR` 16)
  , fromIntegral lb
  )
 where
  hb = network64 rawHb
  lb = network64 rawLb

-- | Constructs a custom `Inet6Address`.
--
--   > inet6AddressFromTuple (0,0,0,0,0,0,0,1) == inet6Loopback
inet6AddressFromTuple
  :: (Word16, Word16, Word16, Word16, Word16, Word16, Word16, Word16)
  -> Inet6Address
inet6AddressFromTuple (w0, w1, w2, w3, w4, w5, w6, w7) =
  Inet6Address (network64 hb) (network64 lb)
 where
  pack a b c d =
    (fromIntegral a `unsafeShiftL` 48)
      .|. (fromIntegral b `unsafeShiftL` 32)
      .|. (fromIntegral c `unsafeShiftL` 16)
      .|. fromIntegral d
  hb = pack w0 w1 w2 w3
  lb = pack w4 w5 w6 w7

-- | @::@
inet6Any :: Inet6Address
inet6Any = inet6AddressFromTuple (0, 0, 0, 0, 0, 0, 0, 0)

-- | @::1@
inet6Loopback :: Inet6Address
inet6Loopback = inet6AddressFromTuple (0, 0, 0, 0, 0, 0, 0, 1)

instance Show Inet6Address where
  showsPrec prec addr =
    showParen (prec > 10) $
      showString $
        L8.unpack $
          B.toLazyByteString $
            mconcat
              [ B.string7 "Inet6Address "
              , B.word16HexFixed g0
              , B.char7 ':'
              , B.word16HexFixed g1
              , B.char7 ':'
              , B.word16HexFixed g2
              , B.char7 ':'
              , B.word16HexFixed g3
              , B.char7 ':'
              , B.word16HexFixed g4
              , B.char7 ':'
              , B.word16HexFixed g5
              , B.char7 ':'
              , B.word16HexFixed g6
              , B.char7 ':'
              , B.word16HexFixed g7
              ]
   where
    (g0, g1, g2, g3, g4, g5, g6, g7) = inet6AddressToTuple addr

instance Storable Inet6Address where
  sizeOf _ = 16
  alignment _ = alignment (undefined :: Word64)
  peek ptr = do
    high <- peek (castPtr ptr :: Ptr Word64)
    low <- peekByteOff ptr 8 :: IO Word64
    return $ Inet6Address high low
  poke ptr (Inet6Address high low) = do
    poke (castPtr ptr) high
    pokeByteOff ptr 8 low

instance Storable Inet6Port where
  sizeOf _ = sizeOf (undefined :: Word16)
  alignment _ = alignment (undefined :: Word16)
  peek ptr = do
    w <- peek (castPtr ptr :: Ptr Word16)
    return $ Inet6Port (network16 w)
  poke ptr (Inet6Port w16) =
    poke (castPtr ptr) (network16 w16)

instance Storable Inet6FlowInfo where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek ptr = do
    w <- peek (castPtr ptr :: Ptr Word32)
    return $ Inet6FlowInfo (network32 w)
  poke ptr (Inet6FlowInfo w32) =
    poke (castPtr ptr) (network32 w32)

instance Storable Inet6ScopeId where
  sizeOf _ = sizeOf (undefined :: Word32)
  alignment _ = alignment (undefined :: Word32)
  peek ptr = do
    w <- peek (castPtr ptr :: Ptr Word32)
    return $ Inet6ScopeId (network32 w)
  poke ptr (Inet6ScopeId w32) =
    poke (castPtr ptr) (network32 w32)

instance Storable (SocketAddress Inet6) where
  sizeOf _ = sockaddrIn6Size
  alignment _ = sockaddrIn6Alignment
  peek ptr =
    SocketAddressInet6
      A.<$> peek (sin6AddrPtr ptr)
      <*> peek (sin6PortPtr ptr)
      <*> peek (sin6FlowInfoPtr ptr)
      <*> peek (sin6ScopeIdPtr ptr)
  poke ptr (SocketAddressInet6 a p f s) = do
    c_memset ptr 0 (fromIntegral c_sizeof_sockaddr_in6)
    when sockaddrIn6HasLen $
      poke (sin6LenPtr ptr) (fromIntegral sockaddrIn6Size :: Word8)
    pokeSaFamily ptr (fromIntegral c_AF_INET6)
    poke (sin6AddrPtr ptr) a
    poke (sin6PortPtr ptr) p
    poke (sin6FlowInfoPtr ptr) f
    poke (sin6ScopeIdPtr ptr) s

-------------------------------------------------------------------------------
-- Address family specific socket options
-------------------------------------------------------------------------------

-- | @IPV6_V6ONLY@
newtype V6Only
  = V6Only Bool
  deriving (Eq, Ord, Show)

instance SocketOption V6Only where
  getSocketOption s =
    V6Only . ((/= 0) :: CInt -> Bool)
      <$> unsafeGetSocketOption s c_IPPROTO_IPV6 c_IPV6_V6ONLY
  setSocketOption s (V6Only o) =
    unsafeSetSocketOption
      s
      c_IPPROTO_IPV6
      c_IPV6_V6ONLY
      (if o then 1 else 0 :: CInt)

sockaddrIn6Size :: Int
sockaddrIn6Size = fromIntegral c_sizeof_sockaddr_in6

sockaddrIn6Alignment :: Int
sockaddrIn6Alignment = fromIntegral c_alignof_sockaddr_in6

sockaddrIn6Sin6FamilyOffset
  , sockaddrIn6Sin6PortOffset
  , sockaddrIn6Sin6FlowInfoOffset
    :: Int
sockaddrIn6Sin6ScopeIdOffset, sockaddrIn6Sin6AddrOffset :: Int
sockaddrIn6Sin6FamilyOffset = fromIntegral c_offset_sockaddr_in6_sin6_family
sockaddrIn6Sin6PortOffset = fromIntegral c_offset_sockaddr_in6_sin6_port
sockaddrIn6Sin6FlowInfoOffset = fromIntegral c_offset_sockaddr_in6_sin6_flowinfo

sockaddrIn6Sin6ScopeIdOffset = fromIntegral c_offset_sockaddr_in6_sin6_scope_id

sockaddrIn6Sin6AddrOffset = fromIntegral c_offset_sockaddr_in6_sin6_addr

sockaddrIn6HasLen :: Bool
sockaddrIn6HasLen = c_has_sockaddr_in6_len /= 0

sockaddrIn6Sin6LenOffset :: Int
sockaddrIn6Sin6LenOffset = fromIntegral c_offset_sockaddr_in6_sin6_len

in6AddrDataOffset :: Int
in6AddrDataOffset = fromIntegral c_offset_in6_addr_s6_addr

sin6PortPtr :: Ptr (SocketAddress Inet6) -> Ptr Inet6Port
sin6PortPtr = (`plusPtr` sockaddrIn6Sin6PortOffset) . castPtr

sin6FlowInfoPtr :: Ptr (SocketAddress Inet6) -> Ptr Inet6FlowInfo
sin6FlowInfoPtr = (`plusPtr` sockaddrIn6Sin6FlowInfoOffset) . castPtr

sin6ScopeIdPtr :: Ptr (SocketAddress Inet6) -> Ptr Inet6ScopeId
sin6ScopeIdPtr = (`plusPtr` sockaddrIn6Sin6ScopeIdOffset) . castPtr

sin6AddrPtr :: Ptr (SocketAddress Inet6) -> Ptr Inet6Address
sin6AddrPtr = (`plusPtr` (sockaddrIn6Sin6AddrOffset + in6AddrDataOffset)) . castPtr

sin6LenPtr :: Ptr (SocketAddress Inet6) -> Ptr Word8
sin6LenPtr = (`plusPtr` sockaddrIn6Sin6LenOffset) . castPtr

saFamily6Size :: Int
saFamily6Size = fromIntegral c_sizeof_sa_family6

pokeSaFamily :: Ptr (SocketAddress Inet6) -> Word16 -> IO ()
pokeSaFamily ptr val =
  case saFamily6Size of
    1 ->
      pokeByteOff
        ptr
        sockaddrIn6Sin6FamilyOffset
        (fromIntegral val :: Word8)
    2 ->
      pokeByteOff
        ptr
        sockaddrIn6Sin6FamilyOffset
        val
    _ -> error "Unsupported sa_family_t size for Inet6 sockets"
