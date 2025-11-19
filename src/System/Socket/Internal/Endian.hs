{-# LANGUAGE CPP #-}

-- |
-- Development support tools like Fourmolu, HLint, and even HLS are known to
-- have severe difficulty with @CPP@. Therefore, for our own convenience, let's
-- avoid @CPP@. When we have to, let's write actual C code and use FFI to call
-- them.
--
-- But then, @WORDS_BIGENDIAN@ is provided by GHC, and we found that the
-- cleanest yet the most performance-friendly way to deal with endianness is
-- this @CPP@.
--
-- Therefore we keep this module minimalistic. Let's not expand it.
module System.Socket.Internal.Endian (
  network16,
  network32,
  network64,
  isBigEndian,
) where

import Data.Word

{-# INLINE network16 #-}
network16 :: Word16 -> Word16

{-# INLINE network32 #-}
network32 :: Word32 -> Word32

{-# INLINE network64 #-}
network64 :: Word64 -> Word64

{-# INLINE isBigEndian #-}
isBigEndian :: Bool

#if defined(WORDS_BIGENDIAN)
network16 = id
network32 = id
network64 = id
isBigEndian = True
#else
network16 = byteSwap16
network32 = byteSwap32
network64 = byteSwap64
isBigEndian = False
#endif
