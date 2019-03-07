{-# LANGUAGE CPP #-}

module Lib
  ( run
  ) where

import qualified Data.Array.Accelerate as A

#ifdef USE_CPU_BACKEND
import qualified Data.Array.Accelerate.LLVM.Native as CPU
#else
import qualified Data.Array.Accelerate.LLVM.PTX as GPU
#endif

-- | Execute an Accelerate program using the GPU unless the @cpu@ flag is
-- specified.
run :: A.Arrays a => A.Acc a -> a
#ifdef USE_CPU_BACKEND
run = CPU.run
#else
run = GPU.run
#endif
