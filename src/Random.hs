{-# LANGUAGE ForeignFunctionInterface #-}

module Random (seed, randomBounded) where

import Data.Word (Word32, Word64)

foreign import ccall "set_seed"
  seed :: Word64 -> Word64 -> IO ()

foreign import ccall "get_random_bounded_u32"
  randomBounded :: Word32 -> IO Word32
