--------------------------------------------------------------------------------
-- |
-- Module      :  System.Socket.Type.SequentialPacket
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
--------------------------------------------------------------------------------
module System.Socket.Type.SequentialPacket where

import System.Socket.Internal.Constants
import System.Socket.Internal.Socket

data SequentialPacket

instance Type SequentialPacket where
  typeNumber _ = c_SOCK_SEQPACKET
