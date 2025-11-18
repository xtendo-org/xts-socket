{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      :  System.Socket.Internal.AddressInfo
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
module System.Socket.Internal.AddressInfo (
  AddressInfo (..),
  HasAddressInfo (..),
  NameInfo (..),
  HasNameInfo (..),
  AddressInfoException (..),
  eaiAgain,
  eaiBadFlags,
  eaiFail,
  eaiFamily,
  eaiMemory,
  eaiNoName,
  eaiSocketType,
  eaiService,
  eaiSystem,
  AddressInfoFlags (..),
  aiAddressConfig,
  aiAll,
  aiCanonicalName,
  aiNumericHost,
  aiNumericService,
  aiPassive,
  aiV4Mapped,
  NameInfoFlags (..),
  niNameRequired,
  niDatagram,
  niNoFullyQualifiedDomainName,
  niNumericHost,
  niNumericService,
) where

import Control.Exception
import Control.Monad
import Data.Bits
import qualified Data.ByteString as BS
import Data.Monoid
import Data.Semigroup as Sem
import Data.Typeable
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Socket.Family.Inet
import System.Socket.Family.Inet6
import System.Socket.Internal.Constants
import System.Socket.Internal.Platform
import System.Socket.Internal.Socket

data AddressInfo f t p
  = AddressInfo
  { addressInfoFlags :: AddressInfoFlags
  , socketAddress :: SocketAddress f
  , canonicalName :: Maybe BS.ByteString
  }

deriving instance (Eq (SocketAddress f)) => Eq (AddressInfo f t p)

deriving instance (Show (SocketAddress f)) => Show (AddressInfo f t p)

-------------------------------------------------------------------------------
-- AddressInfoException
-------------------------------------------------------------------------------

-- | Contains the error code that can be matched against.
--
--   Hint: Use guards or @MultiWayIf@ to match against specific exceptions:
--
--   > if | e == eaiFail -> ...
--   >    | e == eaiNoName -> ...
--   >    | otherwise -> ...
newtype AddressInfoException
  = AddressInfoException CInt
  deriving (Eq, Typeable)

instance Show AddressInfoException where
  show e
    | e == eaiAgain = "eaiAgain"
    | e == eaiBadFlags = "eaiBadFlags"
    | e == eaiFail = "eaiFail"
    | e == eaiFamily = "eaiFamily"
    | e == eaiMemory = "eaiMemory"
    | e == eaiNoName = "eaiNoName"
    | e == eaiService = "eaiService"
    | e == eaiSocketType = "eaiSocketType"
    | e == eaiSystem = "eaiSystem"
    | otherwise =
        let AddressInfoException n = e
         in "AddressInfoException " ++ show n

instance Exception AddressInfoException

-- | > AddressInfoException "Temporary failure in name resolution"
eaiAgain :: AddressInfoException
eaiAgain = AddressInfoException c_EAI_AGAIN

-- | > AddressInfoException "Bad value for ai_flags"
eaiBadFlags :: AddressInfoException
eaiBadFlags = AddressInfoException c_EAI_BADFLAGS

-- | > AddressInfoException "Non-recoverable failure in name resolution"
eaiFail :: AddressInfoException
eaiFail = AddressInfoException c_EAI_FAIL

-- | > AddressInfoException "ai_family not supported"
eaiFamily :: AddressInfoException
eaiFamily = AddressInfoException c_EAI_FAMILY

-- | > AddressInfoException "Memory allocation failure"
eaiMemory :: AddressInfoException
eaiMemory = AddressInfoException c_EAI_MEMORY

-- | > AddressInfoException "No such host is known"
eaiNoName :: AddressInfoException
eaiNoName = AddressInfoException c_EAI_NONAME

-- | > AddressInfoException "Servname not supported for ai_socktype"
eaiService :: AddressInfoException
eaiService = AddressInfoException c_EAI_SERVICE

-- | > AddressInfoException "ai_socktype not supported"
eaiSocketType :: AddressInfoException
eaiSocketType = AddressInfoException c_EAI_SOCKTYPE

-- | > AddressInfoException "System error"
eaiSystem :: AddressInfoException
eaiSystem = AddressInfoException c_EAI_SYSTEM

-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [aiAddressConfig, aiV4Mapped]
newtype AddressInfoFlags
  = AddressInfoFlags CInt
  deriving (Eq, Show, Bits)

instance Sem.Semigroup AddressInfoFlags where
  (AddressInfoFlags a) <> (AddressInfoFlags b) =
    AddressInfoFlags (a .|. b)

instance Data.Monoid.Monoid AddressInfoFlags where
  mempty = AddressInfoFlags 0
  mappend = (Sem.<>)

-- | @AI_ADDRCONFIG@:
aiAddressConfig :: AddressInfoFlags
aiAddressConfig = AddressInfoFlags c_AI_ADDRCONFIG

-- | @AI_ALL@: Return both IPv4 (as v4-mapped IPv6 address) and IPv6 addresses
--  when `aiV4Mapped` is set independent of whether IPv6 addresses exist for
--  this name.
aiAll :: AddressInfoFlags
aiAll = AddressInfoFlags c_AI_ALL

-- | @AI_CANONNAME@:
aiCanonicalName :: AddressInfoFlags
aiCanonicalName = AddressInfoFlags c_AI_CANONNAME

-- | @AI_NUMERICHOST@:
aiNumericHost :: AddressInfoFlags
aiNumericHost = AddressInfoFlags c_AI_NUMERICHOST

-- | @AI_NUMERICSERV@:
aiNumericService :: AddressInfoFlags
aiNumericService = AddressInfoFlags c_AI_NUMERICSERV

-- | @AI_PASSIVE@:
aiPassive :: AddressInfoFlags
aiPassive = AddressInfoFlags c_AI_PASSIVE

-- | @AI_V4MAPPED@: Return mapped IPv4 addresses if no IPv6 addresses could be found
--   or if `aiAll` flag is set.
aiV4Mapped :: AddressInfoFlags
aiV4Mapped = AddressInfoFlags c_AI_V4MAPPED

-- | Use the `Data.Monoid.Monoid` instance to combine several flags:
--
--   > mconcat [niNameRequired, niNoFullyQualifiedDomainName]
newtype NameInfoFlags
  = NameInfoFlags CInt
  deriving (Eq, Show, Bits)

instance Sem.Semigroup NameInfoFlags where
  (NameInfoFlags a) <> (NameInfoFlags b) =
    NameInfoFlags (a .|. b)

instance Monoid NameInfoFlags where
  mempty = NameInfoFlags 0
  mappend = (Sem.<>)

-- | @NI_NAMEREQD@: Throw an exception if the hostname cannot be determined.
niNameRequired :: NameInfoFlags
niNameRequired = NameInfoFlags c_NI_NAMEREQD

-- | @NI_DGRAM@: Service is datagram based (i.e. `System.Socket.Protocol.UDP.UDP`) rather than stream based (i.e. `System.Socket.Protocol.TCP.TCP`).
niDatagram :: NameInfoFlags
niDatagram = NameInfoFlags c_NI_DGRAM

-- | @NI_NOFQDN@: Return only the hostname part of the fully qualified domain name for local hosts.
niNoFullyQualifiedDomainName :: NameInfoFlags
niNoFullyQualifiedDomainName = NameInfoFlags c_NI_NOFQDN

-- | @NI_NUMERICHOST@: Return the numeric form of the host address.
niNumericHost :: NameInfoFlags
niNumericHost = NameInfoFlags c_NI_NUMERICHOST

-- | @NI_NUMERICSERV@: Return the numeric form of the service address.
niNumericService :: NameInfoFlags
niNumericService = NameInfoFlags c_NI_NUMERICSERV

-- | This class is for address families that support name resolution.
class (Family f) => HasAddressInfo f where
  -- | Maps names to addresses (i.e. by DNS lookup).
  --
  --   The operation throws `AddressInfoException`s.
  --
  --   Contrary to the underlying @getaddrinfo@ operation this wrapper is
  --   typesafe and thus only returns records that match the address, type
  --   and protocol encoded in the type. This is the price we have to pay
  --   for typesafe sockets and extensibility.
  --
  --   If you need different types of records, you need to start several
  --   queries. If you want to connect to both IPv4 and IPV6 addresses use
  --   `aiV4Mapped` and use IPv6-sockets.
  --
  --   > getAddressInfo (Just "www.haskell.org") (Just "https") mempty :: IO [AddressInfo Inet Stream TCP]
  --   > > [AddressInfo {addressInfoFlags = AddressInfoFlags 0, socketAddress = SocketAddressInet {inetAddress = InetAddress 162.242.239.16, inetPort = InetPort 443}, canonicalName = Nothing}]
  --
  --   > > getAddressInfo (Just "www.haskell.org") (Just "80") aiV4Mapped :: IO [AddressInfo Inet6 Stream TCP]
  --   > [AddressInfo {
  --   >    addressInfoFlags = AddressInfoFlags 8,
  --   >    socketAddress    = SocketAddressInet6 {inet6Address = Inet6Address 2400:cb00:2048:0001:0000:0000:6ca2:cc3c, inet6Port = Inet6Port 80, inet6FlowInfo = Inet6FlowInfo 0, inet6ScopeId = Inet6ScopeId 0},
  --   >    canonicalName    = Nothing }]
  --
  --   > > getAddressInfo (Just "darcs.haskell.org") Nothing aiV4Mapped :: IO [AddressInfo Inet6 Stream TCP]
  --   > [AddressInfo {
  --   >    addressInfoFlags = AddressInfoFlags 8,
  --   >    socketAddress    = SocketAddressInet6 {inet6Address = Inet6Address 0000:0000:0000:0000:0000:ffff:17fd:e1ad, inet6Port = Inet6Port 0, inet6FlowInfo = Inet6FlowInfo 0, inet6ScopeId = Inet6ScopeId 0},
  --   >    canonicalName    = Nothing }]
  --   > > getAddressInfo (Just "darcs.haskell.org") Nothing mempty :: IO [AddressInfo Inet6 Stream TCP]
  --   > *** Exception: AddressInfoException "Name or service not known"
  getAddressInfo
    :: (Type t, Protocol p)
    => Maybe BS.ByteString
    -> Maybe BS.ByteString
    -> AddressInfoFlags
    -> IO [AddressInfo f t p]

instance HasAddressInfo Inet where
  getAddressInfo = getAddressInfo'

instance HasAddressInfo Inet6 where
  getAddressInfo = getAddressInfo'

getAddressInfo'
  :: forall f t p
   . (Family f, Storable (SocketAddress f), Type t, Protocol p)
  => Maybe BS.ByteString
  -> Maybe BS.ByteString
  -> AddressInfoFlags
  -> IO [AddressInfo f t p]
getAddressInfo' mnode mservice (AddressInfoFlags flags) = do
  alloca $ \resultPtrPtr -> do
    poke resultPtrPtr nullPtr
    allocaBytes sizeofAddrInfo $ \addrInfoPtr -> do
      -- properly initialize the struct
      c_memset addrInfoPtr 0 (fromIntegral c_sizeof_addrinfo)
      poke (ai_flags addrInfoPtr) flags
      poke (ai_family addrInfoPtr) (familyNumber (undefined :: f))
      poke (ai_socktype addrInfoPtr) (typeNumber (undefined :: t))
      poke (ai_protocol addrInfoPtr) (protocolNumber (undefined :: p))
      fnode $ \nodePtr -> do
        fservice $ \servicePtr ->
          bracket
            (c_getaddrinfo nodePtr servicePtr addrInfoPtr resultPtrPtr)
            ( \_ -> do
                resultPtr <- peek resultPtrPtr
                when (resultPtr /= nullPtr) (c_freeaddrinfo resultPtr)
            )
            ( \e ->
                if e == 0
                  then do
                    resultPtr <- peek resultPtrPtr
                    peekAddressInfos resultPtr
                  else do
                    throwIO (AddressInfoException e)
            )
 where
  ai_flags = (`plusPtr` addrinfoFlagsOffset) . castPtr
  ai_family = (`plusPtr` addrinfoFamilyOffset) . castPtr
  ai_socktype = (`plusPtr` addrinfoSockTypeOffset) . castPtr
  ai_protocol = (`plusPtr` addrinfoProtocolOffset) . castPtr
  ai_addr = (`plusPtr` addrinfoAddrOffset) . castPtr
  ai_canonname = (`plusPtr` addrinfoCanonNameOffset) . castPtr
  ai_next = (`plusPtr` addrinfoNextOffset) . castPtr
  fnode = case mnode of
    Just node -> BS.useAsCString node
    Nothing -> \f -> f nullPtr
  fservice = case mservice of
    Just service -> BS.useAsCString service
    Nothing -> \f -> f nullPtr
  peekAddressInfos ptr =
    if ptr == nullPtr
      then return []
      else do
        flag <- peek (ai_flags ptr)
        addr <- peek (ai_addr ptr) >>= peek
        cname <- do
          cnPtr <- peek (ai_canonname ptr)
          if cnPtr == nullPtr
            then return Nothing
            else BS.packCString cnPtr >>= return . Just
        as <- peek (ai_next ptr) >>= peekAddressInfos
        return ((AddressInfo (AddressInfoFlags flag) addr cname) : as)

-- | A `NameInfo` consists of host and service name.
data NameInfo
  = NameInfo
  { hostName :: BS.ByteString
  , serviceName :: BS.ByteString
  }
  deriving (Eq, Show)

-- | This class is for address families that support reverse name resolution.
class (Family f) => HasNameInfo f where
  -- | (Reverse-)map an address back to a human-readable host- and service name.
  --
  --   The operation throws `AddressInfoException`s.
  --
  --   > > getNameInfo (SocketAddressInet inetLoopback 80) mempty
  --   > NameInfo {hostName = "localhost.localdomain", serviceName = "http"}
  getNameInfo :: SocketAddress f -> NameInfoFlags -> IO NameInfo

instance HasNameInfo Inet where
  getNameInfo = getNameInfo'

instance HasNameInfo Inet6 where
  getNameInfo = getNameInfo'

getNameInfo' :: (Storable a) => a -> NameInfoFlags -> IO NameInfo
getNameInfo' addr (NameInfoFlags flags) =
  alloca $ \addrPtr ->
    allocaBytes niMaxHost $ \hostPtr ->
      allocaBytes niMaxServ $ \servPtr -> do
        poke addrPtr addr
        e <-
          c_getnameinfo
            addrPtr
            (fromIntegral $ sizeOf addr)
            hostPtr
            (fromIntegral c_NI_MAXHOST)
            servPtr
            (fromIntegral c_NI_MAXSERV)
            flags
        if e == 0
          then do
            host <- BS.packCString hostPtr
            serv <- BS.packCString servPtr
            return $ NameInfo host serv
          else do
            throwIO (AddressInfoException e)

sizeofAddrInfo :: Int
sizeofAddrInfo = fromIntegral c_sizeof_addrinfo

niMaxHost :: Int
niMaxHost = fromIntegral c_NI_MAXHOST

niMaxServ :: Int
niMaxServ = fromIntegral c_NI_MAXSERV

addrinfoFlagsOffset, addrinfoFamilyOffset, addrinfoSockTypeOffset :: Int
addrinfoProtocolOffset, addrinfoAddrOffset, addrinfoCanonNameOffset :: Int
addrinfoNextOffset :: Int
addrinfoFlagsOffset = fromIntegral c_offset_addrinfo_ai_flags
addrinfoFamilyOffset = fromIntegral c_offset_addrinfo_ai_family
addrinfoSockTypeOffset = fromIntegral c_offset_addrinfo_ai_socktype

addrinfoProtocolOffset = fromIntegral c_offset_addrinfo_ai_protocol

addrinfoAddrOffset = fromIntegral c_offset_addrinfo_ai_addr

addrinfoCanonNameOffset = fromIntegral c_offset_addrinfo_ai_canonname

addrinfoNextOffset = fromIntegral c_offset_addrinfo_ai_next
