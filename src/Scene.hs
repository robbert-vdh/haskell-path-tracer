-- | This module contains the main entry point the for the Accelerate program.
--
-- One important thing to note here are the @(V2 Int, Int)@ tuples. These
-- represent pairs of screen pixel positions and a RNG seed.
--
-- Also note that from here on out everything is living inside of Accelerate, so
-- assume that any common functions that exist in both the prelude and in
-- Accelerate are lifted to work on Accelerate data structures.

module Scene where

import Control.Lens
import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Linear
import Data.Array.Accelerate.Array.Sugar (Elt)

import qualified Prelude as P

import Scene.Objects

-- | The dimensions of the screen. These are hard coded for efficiency's sake
-- even though the window could be resizable with minor adjustments.
screenWidth, screenHeight :: Int32
screenWidth = 800
screenHeight = 600

-- | The dimensions of the screen as a float vector.
screenSize :: Exp (V2 Float)
screenSize =
  constant $ V2 (P.fromIntegral screenWidth) (P.fromIntegral screenHeight)

-- | Render a single sample, combining the previous results with the newly
-- generated sample.
render ::
     Camera
  -> Acc (Matrix (V2 Int, Int))
  -> Acc (Matrix Color)
  -> Acc (Matrix Color)
render camera screen old = zipWith (+) result old
  where
    result = undefined

-- | Calculate the origin and directions of the primary rays based on a camera
-- and a matrix of screen pixel positions. These positions should be in the
-- format @V2 <0 .. screenWidth> <0 .. screenHeight>@.
primaryRays :: Camera -> Acc (Matrix (V2 Int, Int)) -> Acc (Matrix (Ray, Int))
primaryRays camera = map transform
  where
    viewMatrix = undefined
    transform :: Exp (V2 Int, Int) -> Exp (Ray, Int)
    transform e =
      let rasterPos = vecToFloat $ e ^. _1
          -- Screen space is the space where both X and Y coordinates lie within
          -- the @[-1, 1]@ interval
          screenPos = rasterPos / screenSize * 2.0 - 1.0
          seed = e ^. _2
       in undefined

-- | Convert an integer vector to a float vector. This is only used when
-- converting between rasterization and world spaces.
vecToFloat ::
     (Functor f, Elt (f Int), Elt (f Float)) => Exp (f Int) -> Exp (f Float)
vecToFloat = fmap toFloating
