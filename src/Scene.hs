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
import Data.Array.Accelerate.Control.Lens hiding (transform)
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Linear

import qualified Prelude as P ()

import Intersection
import Scene.Objects
import Scene.World
import Util

-- | Render a single sample, combining the previous results with the newly
-- generated sample.
render ::
     Exp Camera
  -> Acc (Matrix (V2 Int))
  -> Acc (Matrix (Color, Word32))
  -> Acc (Matrix (Color, Word32))
render camera screen acc = zipWith (\(T2 new seed) (T2 old _) -> T2 (new + old) seed) result acc
  where
    rays = primaryRays camera screen
    seeds = map snd acc
    result = map (traceRay 20 getObjects) $ zip rays seeds

-- | Calculate the origin and directions of the primary rays based on a camera
-- and a matrix of screen pixel positions. These positions should be in the
-- format @V2 <0 .. screenWidth> <0 .. screenHeight>@.
primaryRays ::
     Exp Camera -> Acc (Matrix (V2 Int)) -> Acc (Matrix RayF)
primaryRays ~(Camera' cPos cDir cFov) = map transform
  where
    -- | The distance between the camera nd the virtual screen plane
    --
    -- TODO: This distance should be calculated based on the FoV
    screenDistance :: Exp Float
    screenDistance = 0.05
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
      let center = cPos + cDir ^* screenDistance
          centerOffset = center - cPos
          rightOffset = centerOffset `cross` up
          topOffset = (cDir `cross` rightOffset) ^/ screenAspect
       in (center, topOffset, rightOffset)

    transform :: Exp (V2 Int) -> Exp RayF
    transform (vecToFloat -> rasterPos) =
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
       in Ray' cPos rayDir

-- ** Single ray, multiple objects

-- | Calculate the amount of light that a given ray would collect when shot into
-- the scene. In other words, calculate what color the pixel that corresponds to
-- the ray should be.
--
-- Because if the way Accelerate is structured, we had to invert the recursive
-- flow. Instead of returning @emittance + (brdf * traceRay (limit - 1) scene
-- nextRay)@, we return @old_result + (emittance * multiplier)@, where
-- multiplier is a comulative product of the BRDFs.
--
-- TODO: The BRDF is rather simplistic and should be expanded upon
-- TODO: The BRDF should probably take the material of non-illuminating props
--       into account (a red ball should not be able to reflect green light)
traceRay :: Exp Int -> Scene -> Exp (RayF, Word32) -> Exp (Color, Word32)
traceRay limit scene primaryRay =
  let T3 (T2 _ seed) result _ = iterate limit go (T3 primaryRay (V3' 0 0 0) 1.0)
   in T2 result seed
  where
    go :: Exp ((RayF, Word32), Color, Float) -> Exp ((RayF, Word32), Color, Float)
    go (T3 (T2 ray seed) result multiplier) =
      let nextHit = closestIntersection scene ray
       in if nearZero multiplier || isNothing nextHit
            then T3 (T2 ray seed) result 0.0
            else let T2 (Ray' intersection iNormal) iMaterial = fromJust nextHit
                     T2 rotationVector nextSeed = genVec seed
                     nextDirection = rotate (anglesToQuaternion pi rotationVector) iNormal
                     nextRay = Ray' (intersection + nextDirection ^* epsilon) nextDirection

                     emittance =
                       (iMaterial ^. color) ^* (iMaterial ^. illuminance)
                     brdf =
                       2.0 * (iMaterial ^. specularity) *
                       ((nextRay ^. direction) `dot` iNormal)
                  in T3
                       (T2 nextRay nextSeed)
                       (result + (emittance ^* multiplier))
                       (multiplier * brdf)

closestIntersection :: Scene -> Exp RayF -> Exp (Maybe (Normal, Material))
closestIntersection scene ray =
  fmap snd $
  expMinWith (maybe infinite fst) $
  mapScene (\p -> pairHit p <$> distanceTo ray p) scene
  where
    pairHit p t = T2 t (hit ray t p)

-- | Max possible value for a float, usefull as distance if there is not hit.
infinite :: Exp Float
infinite = encodeFloat 16777215 104

-- | A small offset used to prevent rays from intersecting with the same object
-- it last intersected with.
epsilon :: Exp Float
epsilon = 0.002
