{-# LANGUAGE CPP #-}
module Lib
  ( run
  , run1
  , runN
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

#ifdef USE_CPU_BACKEND
run1 :: (A.Arrays a, A.Arrays b) => (A.Acc a -> A.Acc b) -> a -> b
run1 = CPU.run1
#else
run1 :: (A.Arrays a, A.Arrays b) => (A.Acc a -> A.Acc b) -> a -> b
run1 = GPU.run1
#endif

#ifdef USE_CPU_BACKEND
runN :: CPU.Afunction f => f -> CPU.AfunctionR f
runN = CPU.runN
#else
runN :: GPU.Afunction f => f -> GPU.AfunctionR f
runN = GPU.runN
#endif
