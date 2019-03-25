{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RebindableSyntax #-}
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
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Linear

import qualified Prelude as P

import Data.Array.Accelerate.Linear.Projection
import Intersection
import Scene.Objects
import Scene.World
import Util

-- | Render a single sample, combining the previous results with the newly
-- generated sample.
render ::
     Exp Camera
  -> Acc (Matrix (V2 Int, Int))
  -> Acc (Matrix Color)
  -> Acc (Matrix Color)
render camera screen = zipWith (+) result
  where
    hasHit ray = expAny isJust $ mapScene (distanceTo ray) getBasicObjects
    -- TODO: Do some actual rendering here
    dist ray =
      expMin $
      P.map (fromMaybe infinite) $ mapScene (distanceTo ray) getBasicObjects
    result =
      map
        (\(T2 r _) ->
           if hasHit r
             then V3' 0 (1 - tanh (dist r / 10) * 0.9) 0
             else V3' 1 0 0) $
      primaryRays camera screen

-- | Calculate the origin and directions of the primary rays based on a camera
-- and a matrix of screen pixel positions. These positions should be in the
-- format @V2 <0 .. screenWidth> <0 .. screenHeight>@.
primaryRays ::
     Exp Camera -> Acc (Matrix (V2 Int, Int)) -> Acc (Matrix (RayF, Int))
primaryRays ~(Camera' cPos cDir (toFloating -> cFov)) = map transform
  where
    verticalFov :: Exp Float
    -- TODO: This value is too high
    verticalFov = 2.0 * atan (tan ((cFov * (pi / 180.0)) / 2.0) * screenAspect)
    viewMatrix :: Exp (M44 Float)
    viewMatrix =
      -- TODO: This lookAt is not quite right apparently, but it works fine when
      --       the camera is in the origin so we should fix this once we have
      --       user input
      lookAtScratch cPos (cPos + cDir) (V3' 0.0 1.0 0.0)
      !*!
      infinitePerspective verticalFov screenAspect 0.001

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
          nearPoint = normalize $
            viewMatrix !* point (V3' screenX (negate screenY) 0.0)
          farPoint = normalize $
            viewMatrix !* point (V3' screenX (negate screenY) 1.0)

          -- TODO: This noramlize is not necesary and is here purely for
          --       debugging purposes
          rayDir :: Exp (V3 Float)
          rayDir = normalize $ fromHomogeneous $ nearPoint - farPoint

          ray :: Exp RayF
          ray = Ray' cPos rayDir
       in T2 ray seed

-- ** Single ray, multiple objects
-- | A function which calculates the resulting color given a bounce limit,
-- a scene and a ray.
traceRay :: Exp Int -> Acc Scene -> Exp RayF -> Exp Color
traceRay limit scene ~(Ray' o d) = go limit o d
  where
    go :: Exp Int -> Exp Position -> Exp Direction -> Exp Color
    -- If not, check if a ray from position @pos@ going in direction @dir@
    -- itersects with anything in the scene. If it does calculate reflection and
    -- recursivly call this function. If nothing gets hit or the bounce limit is
    -- reached, return black.
    go bounces pos dir =
      if bounces == 0
        then V3' 0.0 0.0 0.0
        else undefined

-- | Find the nearest hit for a ray given a array of objects
--
-- we can map distanceTo{Sphere,Plane} on all opjects and use the `justs` function to
-- extract the Just Exps.
castRay ::
  (Exp obj -> Exp RayF -> Exp (Maybe Float))
  -> Acc (Vector obj) -- the list of objects (must be the same type)
  -> Exp RayF -- Ray with start and direction
  -> Exp (Maybe (Float, obj)) -- Return hit?, distance and which object has been hit
castRay = undefined

-- | Max possible value for a float, usefull as distance if there is not hit.
infinite :: Exp Float
infinite = encodeFloat 16777215 104
