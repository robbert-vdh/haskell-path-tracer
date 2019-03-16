{-# LANGUAGE ViewPatterns #-}

-- | This module contains the main entry point the for the Accelerate program.
--
-- One important thing to note here are the @(V2 Int, Int)@ tuples. These
-- represent pairs of screen pixel positions and a RNG seed.
--
-- Also note that from here on out everything is living inside of Accelerate, so
-- assume that any common functions that exist in both the prelude and in
-- Accelerate are lifted to work on Accelerate data structures.

module Scene where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Linear
import Data.Array.Accelerate.Array.Sugar (Elt)

import qualified Prelude as P

import Scene.Objects
import Data.Array.Accelerate.Linear.Projection

-- | The dimensions of the screen. These are hard coded for efficiency's sake
-- even though the window could be resizable with minor adjustments.
screenWidth, screenHeight :: Int32
screenWidth = 800
screenHeight = 600

-- | The dimensions of the screen as a float vector. Note that the screen height
-- has been inverted here as the @y@ axis changes orientation when converting
-- between rasterization and screen spaces.
screenSize :: Exp (V2 Float)
screenSize =
  constant $
  V2 (P.fromIntegral screenWidth) (P.fromIntegral $ negate screenHeight)

-- | Render a single sample, combining the previous results with the newly
-- generated sample.
render ::
     Exp Camera
  -> Acc (Matrix (V2 Int, Int))
  -> Acc (Matrix Color)
  -> Acc (Matrix Color)
render camera screen = zipWith (+) result
    -- TODO: Do some actual rendering here
  where
    result =
      map (\(T2 (Ray' _ d) _) -> (d + 1.0) / 1.0) $
      primaryRays camera screen

-- | Calculate the origin and directions of the primary rays based on a camera
-- and a matrix of screen pixel positions. These positions should be in the
-- format @V2 <0 .. screenWidth> <0 .. screenHeight>@.
primaryRays :: Exp Camera -> Acc (Matrix (V2 Int, Int)) -> Acc (Matrix (RayF, Int))
primaryRays ~(Camera' cPos cDir cFov) = map transform
  where
    -- TODO: Include the perspective here
    viewMatrix :: Exp (M44 Float)
    viewMatrix = lookAt (cPos + cDir) cPos (V3' 0.0 1.0 0.0)
    transform :: Exp (V2 Int, Int) -> Exp (RayF, Int)
    transform (T2 (vecToFloat -> rasterPos) seed) =
      let -- Screen space is the space where both X and Y coordinates lie within
          -- the @[-1, 1]@ interval. Here @(-1, -1)@ is the bottom left and @(1,
          -- 1)@ is the top right corner. Because of this the @y@ axis has to be
          -- inverted during the computation. This has already been accounted
          -- for in 'screenSize', hence why the Y-axis value gets increased by
          -- two.
          V2' screenX screenY =
            rasterPos / screenSize * 2.0 + constant (V2 (-1.0) 1.0)

          nearPoint, farPoint :: Exp (V4 Float)
          nearPoint = point $ V3' screenX screenY 1.0
          farPoint = point $ V3' screenX screenY (-1.0)
          rayDir :: Exp (V3 Float)
          rayDir = normalizePoint $
            (viewMatrix !* farPoint) - (viewMatrix !* nearPoint)

          ray :: Exp RayF
          ray = Ray' cPos rayDir
       in T2 ray seed

-- | Convert an integer vector to a float vector. This is only used when
-- converting between rasterization and world spaces.
vecToFloat ::
     (Functor f, Elt (f Int), Elt (f Float)) => Exp (f Int) -> Exp (f Float)
vecToFloat = fmap toFloating
