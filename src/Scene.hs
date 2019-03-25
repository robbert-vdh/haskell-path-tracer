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
    hasHit ray = expAny isJust $ mapScene (distanceTo ray) getObjects
    -- TODO: Do some actual rendering here
    dist ray =
      expMin $
      P.map (fromMaybe infinite) $ mapScene (distanceTo ray) getObjects
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
primaryRays ~(Camera' cPos cDir cFov) = map transform
  where
    -- | The distance between the camera nd the virtual screen plane
    --
    -- TODO: This distance should be calculated based on the FoV
    epsilon :: Exp Float
    epsilon = 0.05
    -- | The direction in world space that represents looking upwards. This is
    -- used for calculating perspectives.
    up :: Exp (V3 Float)
    up = V3' 0 1 0

    -- | The coordinates of a virtual screen plane. Instead of using regular
    -- matrix transformations, we'll simply map every pixel on the screen to a
    -- point on this virtual plane. We can then simply calculate the ray's
    -- direction by drawing a line between the camera's origin and the point
    -- we've calculated.
    planeCenter, planeTopOffset, planeRightOffset :: Exp (V3 Float)
    (planeCenter, planeTopOffset, planeRightOffset) =
      let center = cPos + cDir ^* epsilon
          centerOffset = center - cPos
          rightOffset = centerOffset `cross` up
          topOffset = (cDir `cross` rightOffset) ^/ screenAspect
       in (center, topOffset, rightOffset)

    transform :: Exp (V2 Int, Int) -> Exp (RayF, Int)
    transform (T2 (vecToFloat -> rasterPos) seed) =
      let -- Screen space is the space where both X and Y coordinates lie within
          -- the @[-1, 1]@ interval. Here @(-1, -1)@ is the bottom left and @(1,
          -- 1)@ is the top right corner. Because of this the @y@ axis has to be
          -- inverted during the computation. This has already been accounted
          -- for in 'screenSize', hence why the Y-axis value gets increased by
          -- two.
          V2' screenX screenY = rasterPos / screenSize * 2.0 + V2' (-1.0) 1.0

          virtualPoint :: Exp Position
          virtualPoint = planeCenter + (planeRightOffset ^* screenX) + (planeTopOffset ^* screenY)
          rayDir :: Exp (V3 Float)
          rayDir = normalize $ virtualPoint - cPos
          ray :: Exp RayF
          ray = Ray' cPos rayDir
       in T2 ray seed

-- ** Single ray, multiple objects
-- | A function which calculates the resulting color given a bounce limit,
-- a scene and a ray.
traceRay :: Exp Int -> Scene -> Exp RayF -> Exp Color
traceRay limit scene = go limit
  where
    go :: Exp Int -> Exp RayF -> Exp Color
    -- If not, check if a ray from position @pos@ going in direction @dir@
    -- itersects with anything in the scene. If it does calculate reflection and
    -- recursivly call this function. If nothing gets hit or the bounce limit is
    -- reached, return black.
    go bounces ray =
      if bounces == 0
        then V3' 0.0 0.0 0.0
        else let
          f :: forall p. Primitive p => Exp p -> Exp (Float, (RayF, Material))
          f p = T2 dist (hit ray dist p)
            where
              dist = fromMaybe infinite (distanceTo ray p)

          closest_hit :: Exp (RayF, Material)
          closest_hit = snd $ expMinWith fst $ mapScene f scene

          reflection = go (bounces - 1) (fst closest_hit)

          in V3' 0.0 0.0 0.0

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
