--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      :  System.Socket.Protocol.UDP
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
module System.Socket.Protocol.UDP where

import System.Socket.Internal.Constants
import System.Socket.Internal.Socket

data UDP

instance Protocol UDP where
  protocolNumber _ = c_IPPROTO_UDP
