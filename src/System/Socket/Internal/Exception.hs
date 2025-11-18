{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      :  System.Socket.Internal.Exception
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
module System.Socket.Internal.Exception (
  SocketException (..),
  eOk,
  eInterrupted,
  eBadFileDescriptor,
  ePermissionDenied,
  eInvalid,
  ePipe,
  eWouldBlock,
  eAgain,
  eNotSocket,
  eDestinationAddressRequired,
  eMessageSize,
  eProtocolType,
  eNoProtocolOption,
  eProtocolNotSupported,
  eSocketTypeNotSupported,
  eOperationNotSupported,
  eProtocolFamilyNotSupported,
  eAddressFamilyNotSupported,
  eAddressInUse,
  eAddressNotAvailable,
  eNetworkDown,
  eNetworkUnreachable,
  eNetworkReset,
  eConnectionAborted,
  eConnectionReset,
  eNoBufferSpace,
  eIsConnected,
  eNotConnected,
  eShutdown,
  eTooManyReferences,
  eTimedOut,
  eConnectionRefused,
  eHostDown,
  eHostUnreachable,
  eAlready,
  eInProgress,
) where

import Control.Exception
import Data.Typeable
import Foreign.C.Types
import System.Socket.Internal.Constants

-- | Contains the error code that can be matched against.
--
--   Hint: Use guards or @MultiWayIf@ to match against specific exceptions:
--
--   > if | e == eAddressInUse -> ...
--   >    | e == eAddressNotAvailable -> ...
--   >    | otherwise -> ...
newtype SocketException
  = SocketException CInt
  deriving (Typeable, Eq, Ord)

instance Exception SocketException

instance Show SocketException where
  show e
    | e == eOk = "eOk"
    | e == eInterrupted = "eInterrupted"
    | e == eBadFileDescriptor = "eBadFileDescriptor"
    | e == ePermissionDenied = "ePermissionDenied"
    | e == eInvalid = "eInvalid"
    | e == ePipe = "ePipe"
    | e == eWouldBlock = "eWouldBlock"
    | e == eAgain = "eAgain"
    | e == eNotSocket = "eNotSocket"
    | e == eDestinationAddressRequired = "eDestinationAddressRequired"
    | e == eMessageSize = "eMessageSize"
    | e == eProtocolType = "eProtocolType"
    | e == eNoProtocolOption = "eNoProtocolOption"
    | e == eProtocolNotSupported = "eProtocolNotSupported"
    | e == eSocketTypeNotSupported = "eSocketTypeNotSupported"
    | e == eOperationNotSupported = "eOperationNotSupported"
    | e == eProtocolFamilyNotSupported = "eProtocolFamilyNotSupported"
    | e == eAddressFamilyNotSupported = "eAddressFamilyNotSupported"
    | e == eAddressInUse = "eAddressInUse"
    | e == eAddressNotAvailable = "eAddressNotAvailable"
    | e == eNetworkDown = "eNetworkDown"
    | e == eNetworkUnreachable = "eNetworkUnreachable"
    | e == eNetworkReset = "eNetworkReset"
    | e == eConnectionAborted = "eConnectionAborted"
    | e == eConnectionReset = "eConnectionReset"
    | e == eNoBufferSpace = "eNoBufferSpace"
    | e == eIsConnected = "eIsConnected"
    | e == eNotConnected = "eNotConnected"
    | e == eShutdown = "eShutdown"
    | e == eTooManyReferences = "eTooManyReferences"
    | e == eTimedOut = "eTimedOut"
    | e == eConnectionRefused = "eConnectionRefused"
    | e == eHostDown = "eHostDown"
    | e == eHostUnreachable = "eHostUnreachable"
    | e == eAlready = "eAlready"
    | e == eInProgress = "eInProgress"
    | otherwise =
        let SocketException n = e
         in "SocketException " ++ show n

-- | No error.
eOk :: SocketException
eOk = SocketException c_SEOK

-- | Interrupted system call.
--
--   NOTE: This exception shall not be thrown by any public operation in this
--   library, but is handled internally.
eInterrupted :: SocketException
eInterrupted = SocketException c_SEINTR

-- | Bad file descriptor.
eBadFileDescriptor :: SocketException
eBadFileDescriptor = SocketException c_SEBADF

-- | Permission denied.
ePermissionDenied :: SocketException
ePermissionDenied = SocketException c_SEACCES

-- | Invalid argument.
eInvalid :: SocketException
eInvalid = SocketException c_SEINVAL

-- | Broken pipe.
ePipe :: SocketException
ePipe = SocketException c_SEPIPE

-- | Resource temporarily unavailable.
--
--   NOTE: This exception shall not be thrown by any public operation in this
--   library, but is handled internally.
eWouldBlock :: SocketException
eWouldBlock = SocketException c_SEWOULDBLOCK

-- | Resource temporarily unavailable.
eAgain :: SocketException
eAgain = SocketException c_SEAGAIN

-- | Socket operation on non-socket.
--
--  NOTE: This should be ruled out by the type system.
eNotSocket :: SocketException
eNotSocket = SocketException c_SENOTSOCK

-- | Destination address required.
eDestinationAddressRequired :: SocketException
eDestinationAddressRequired = SocketException c_SEDESTADDRREQ

-- | Message too long.
eMessageSize :: SocketException
eMessageSize = SocketException c_SEMSGSIZE

-- | Protocol wrong type for socket.

--  NOTE: This should be ruled out by the type system.
eProtocolType :: SocketException
eProtocolType = SocketException c_SEPROTOTYPE

-- | Protocol not available.
eNoProtocolOption :: SocketException
eNoProtocolOption = SocketException c_SENOPROTOOPT

-- | Protocol not supported.
eProtocolNotSupported :: SocketException
eProtocolNotSupported = SocketException c_SEPROTONOSUPPORT

-- | Socket type not supported.
eSocketTypeNotSupported :: SocketException
eSocketTypeNotSupported = SocketException c_SESOCKTNOSUPPORT

-- | Operation not supported.
eOperationNotSupported :: SocketException
eOperationNotSupported = SocketException c_SEOPNOTSUPP

-- | Protocol family not supported.
eProtocolFamilyNotSupported :: SocketException
eProtocolFamilyNotSupported = SocketException c_SEPFNOSUPPORT

-- | Address family not supported by protocol.
eAddressFamilyNotSupported :: SocketException
eAddressFamilyNotSupported = SocketException c_SEAFNOSUPPORT

-- | Address already in use.
eAddressInUse :: SocketException
eAddressInUse = SocketException c_SEADDRINUSE

-- | Cannot assign requested address.
eAddressNotAvailable :: SocketException
eAddressNotAvailable = SocketException c_SEADDRNOTAVAIL

-- | Network is down.
eNetworkDown :: SocketException
eNetworkDown = SocketException c_SENETDOWN

-- | Network is unreachable.
eNetworkUnreachable :: SocketException
eNetworkUnreachable = SocketException c_SENETUNREACH

-- | Network dropped connection on reset.
eNetworkReset :: SocketException
eNetworkReset = SocketException c_SENETRESET

-- | Software caused connection abort.
eConnectionAborted :: SocketException
eConnectionAborted = SocketException c_SECONNABORTED

-- | Connection reset by peer.
eConnectionReset :: SocketException
eConnectionReset = SocketException c_SECONNRESET

-- | No buffer space available.
eNoBufferSpace :: SocketException
eNoBufferSpace = SocketException c_SENOBUFS

-- | Transport endpoint is already connected.
eIsConnected :: SocketException
eIsConnected = SocketException c_SEISCONN

-- | Transport endpoint is not connected.
eNotConnected :: SocketException
eNotConnected = SocketException c_SENOTCONN

-- | Cannot send after transport endpoint shutdown.
eShutdown :: SocketException
eShutdown = SocketException c_SESHUTDOWN

-- | Too many references: cannot splice.
eTooManyReferences :: SocketException
eTooManyReferences = SocketException c_SETOOMANYREFS

-- | Connection timed out.
eTimedOut :: SocketException
eTimedOut = SocketException c_SETIMEDOUT

-- | Connection refused.
eConnectionRefused :: SocketException
eConnectionRefused = SocketException c_SECONNREFUSED

-- | Host is down.
eHostDown :: SocketException
eHostDown = SocketException c_SEHOSTDOWN

-- | No route to host.
eHostUnreachable :: SocketException
eHostUnreachable = SocketException c_SEHOSTUNREACH

-- | Operation already in progress.
--
--   NOTE: This exception shall not be thrown by any public operation in this
--   library, but is handled internally.
eAlready :: SocketException
eAlready = SocketException c_SEALREADY

-- | Operation now in progress
eInProgress :: SocketException
eInProgress = SocketException c_SEINPROGRESS
