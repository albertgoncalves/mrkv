{-# LANGUAGE ForeignFunctionInterface #-}

module Random (setSeed, getRandom) where

import Data.Word (Word64)
import Foreign.C.Types (CFloat (..))

foreign import ccall "set_seed" c_set_seed :: Word64 -> Word64 -> IO ()

setSeed :: Word64 -> Word64 -> IO ()
setSeed = c_set_seed

foreign import ccall "get_random_f32" c_get_random_f32 :: IO CFloat

getRandom :: IO Float
getRandom = realToFrac <$> c_get_random_f32
