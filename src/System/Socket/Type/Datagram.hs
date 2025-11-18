--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- |
-- Module      :  System.Socket.Type.Datagram
-- Copyright   :  (c) Lars Petersen 2015
-- License     :  MIT
--
-- Maintainer  :  info@lars-petersen.net
-- Stability   :  experimental
module System.Socket.Type.Datagram where

import System.Socket.Internal.Constants
import System.Socket.Internal.Socket

data Datagram

instance Type Datagram where
  typeNumber _ = c_SOCK_DGRAM
