-- | This module contains the main entry point the for the Accelerate program.
--
-- One important thing to note here are the @(V2 Int, Int)@ tuples. These
-- represent pairs of screen pixel positions and a RNG seed.

module Scene where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Linear as A

import Scene.Objects

-- | Render a single sample, combining the previous results with the newly
-- generated sample.
render ::
     Camera
  -> Acc (Matrix (V2 Int, Int))
  -> Acc (Matrix Color)
  -> Acc (Matrix Color)
render camera screen old = A.zipWith (+) result old
  where
    result = undefined

-- | Dit is helemaal leip, zelfs al probeert het gewoon te doen:
--
-- https://hackage.haskell.org/package/linear-1.20.8/docs/Linear-Projection.html
screenToViewRays :: Camera -> Acc (Matrix (V2 Int, Int)) -> Acc (Matrix (Direction, Int))
screenToViewRays = undefined
