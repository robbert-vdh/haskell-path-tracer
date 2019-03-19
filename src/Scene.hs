{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}

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

-- | The aspect ratio of the output. Used in the FOV and perspective
-- calculations.
screenAspect :: Exp Float
screenAspect = P.fromIntegral screenWidth / P.fromIntegral screenHeight

-- | The dimensions of the screen as a float vector. Note that the screen height
-- has been inverted here as the @y@ axis changes orientation when converting
-- between rasterization and screen spaces.
screenSize :: Exp (V2 Float)
screenSize =
  V2' (P.fromIntegral screenWidth) (P.fromIntegral $ negate screenHeight)

-- | Render a single sample, combining the previous results with the newly
-- generated sample.
render ::
     Exp Camera
  -> Acc (Matrix (V2 Int, Int))
  -> Acc (Matrix Color)
  -> Acc (Matrix Color)
render camera screen = zipWith (+) result
  where
    -- TODO: Do some actual rendering here
    result =
      map (\(T2 (Ray' _ d) _) -> (d + 1.0) / 2.0) $
      primaryRays camera screen

-- | Calculate the origin and directions of the primary rays based on a camera
-- and a matrix of screen pixel positions. These positions should be in the
-- format @V2 <0 .. screenWidth> <0 .. screenHeight>@.
primaryRays ::
     Exp Camera
  -> Acc (Matrix (V2 Int, Int))
  -> Acc (Matrix (RayF, Int))
primaryRays ~(Camera' cPos cDir (toFloating -> cFov)) = map transform
  where
    verticalFov :: Exp Float
    verticalFov = 2.0 * atan (tan ((cFov * (pi / 180.0)) / 2.0) * screenAspect)
    viewMatrix :: Exp (M44 Float)
    viewMatrix =
      infinitePerspective verticalFov screenAspect 0.01 !*!
      -- TODO: Right now cDir is a normalized looking direction, but this should
      --       become a quaternion once we add user input
      lookAt (cPos + cDir) cPos (V3' 0.0 1.0 0.0)

    transform :: Exp (V2 Int, Int) -> Exp (RayF, Int)
    transform (T2 (vecToFloat -> rasterPos) seed) =
      let -- Screen space is the space where both X and Y coordinates lie within
          -- the @[-1, 1]@ interval. Here @(-1, -1)@ is the bottom left and @(1,
          -- 1)@ is the top right corner. Because of this the @y@ axis has to be
          -- inverted during the computation. This has already been accounted
          -- for in 'screenSize', hence why the Y-axis value gets increased by
          -- two.
          V2' screenX screenY = rasterPos / screenSize * 2.0 + V2' (-1.0) 1.0

          nearPoint, farPoint :: Exp (V4 Float)
          nearPoint = viewMatrix !* point (V3' screenX screenY 1.0)
          farPoint = viewMatrix !* point (V3' screenX screenY (-1.0))
          -- TODO: This noramlize is not necesary and is here purely for
          --       debugging purposes

          nearPointV3, farPointV3 :: Exp (V3 Float)
          nearPointV3 = normalizePoint nearPoint
          farPointV3 = normalizePoint farPoint

          rayDir :: Exp (V3 Float)
          rayDir = normalize $ (nearPointV3 - farPointV3)

          ray :: Exp RayF
          ray = Ray' nearPointV3 rayDir
       in T2 ray seed

-- | Convert an integer vector to a float vector. This is only used when
-- converting between rasterization and world spaces.
vecToFloat ::
     (Functor f, Elt (f Int), Elt (f Float)) => Exp (f Int) -> Exp (f Float)
vecToFloat = fmap toFloating

-- ** Single ray, multiple objects
-- | A function which calculates the resulting color given a bounce limit,
-- a scene and a ray.
traceRay :: Int -> Acc Scene -> Exp RayF -> Exp Color
traceRay limit scene (Ray' o d) = go limit o d
  where
    go :: Int -> Exp Position -> Exp Direction -> Exp Color
    -- When bounce limit is reached return black
    go 0 _ _ = V3' 0.0 0.0 0.0
    -- If not, check if a ray from position @pos@ going in direction @dir@
    -- itersects with anything in the scene. If it does calculate reflection
    -- and recursivly call this function. If nothing gets hit, return black
    go bounces pos dir = undefined

-- | Find the nearest hit for a ray given a array of objects
castRay :: forall obj. Elt obj
    => (Exp obj -> Exp RayF -> Exp(Bool, Float))
    -> Exp obj           -- Dummy object, returned if no hit (maybe does not exist)
    -> Acc (Vector obj)  -- the list of objects (must be the same type)
    -> Exp RayF          -- Ray with start and direction
    -> Exp (Bool, Float, obj) -- Return hit?, distance and which object has been hit
castRay = undefined

-- ** Single ray, single object
-- | Get intersection point, normal, color and shine for a sphere hit.
hitSphere :: Exp Sphere -> Exp Float -> Exp RayF -> Exp (Position, Direction, Color, Float)
hitSphere = undefined

-- | Get the intersection point, normal, color and shine of a plane hit.
hitPlane :: Exp Plane -> Exp Float -> Exp RayF -> Exp (Position, Direction, Color, Float)
hitPlane = undefined

-- | Distance to a Spere if it intersects, bool says if there is a hit, float the distance.
distanceToSphere :: Exp Sphere -> Exp RayF -> Exp (Bool, Float)
distanceToSphere = undefined

-- | Distance to a Plane if it intersects, bool says if there is a hit, float the distance.
distanceToPlane :: Exp Plane -> Exp RayF -> Exp (Bool, Float)
distanceToPlane = undefined

-- | Max possible value for a float, usefull as distance if there is not hit
infinite :: Exp Float
infinite = encodeFloat 16777215 104
