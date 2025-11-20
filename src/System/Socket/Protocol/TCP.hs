-- |
-- Module      :  System.Socket.Protocol.TCP
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
module System.Socket.Protocol.TCP where

import Foreign.C.Types
import System.Socket.Internal.Constants
import System.Socket.Internal.Socket
import System.Socket.Internal.SocketOption

data TCP

instance Protocol TCP where
  protocolNumber _ = c_IPPROTO_TCP

-- | If set to True, disable the Nagle's algorithm.
--
--  - Also know as @TCP_NODELAY@.
newtype NoDelay
  = NoDelay Bool
  deriving (Eq, Ord, Show)

instance SocketOption NoDelay where
  getSocketOption s =
    (NoDelay . (/= 0) :: CInt -> NoDelay)
      `fmap` unsafeGetSocketOption s c_IPPROTO_TCP c_TCP_NODELAY
  setSocketOption s (NoDelay o) =
    unsafeSetSocketOption
      s
      c_IPPROTO_TCP
      c_TCP_NODELAY
      (if o then 1 else 0 :: CInt)
