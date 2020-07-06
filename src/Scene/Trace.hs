{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains the main entry point the for the Accelerate program.
--
-- One important thing to note here are the @(V2 Int, Int)@ tuples. These
-- represent pairs of screen pixel positions and a RNG seed.
--
-- Also note that from here on out everything is living inside of Accelerate, so
-- assume that any common functions that exist in both the prelude and in
-- Accelerate are lifted to work on Accelerate data structures.

module Scene.Trace where

import Data.Array.Accelerate
import Data.Array.Accelerate.Control.Lens hiding (transform)
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Data.Functor
import Data.Array.Accelerate.Linear

import qualified Prelude as P ()

import Scene.Intersection
import Scene.Objects
import Scene.World
import Util

-- | Render a single sample, combining the previous results with the newly
-- generated sample. This result is an array of color values summed over the
-- entire runtime of the application (or until the rendering gets reset). The
-- sums can then be divided by the number of samples directly in an OpenGL
-- fragment shader to obtain the final per-pixel averages.
--
-- The general idea behind this function is that the result of a previous
-- calculation can the passed on to the next function call. This way we can also
-- reuse our RNG seeds for multiple samples.
render ::
     Acc (Matrix (V2 Int)) -- ^ Screen pixel coordinates
  -> Acc (Scalar Camera)
  -> Acc (Matrix (Color, Word32)) -- ^ Accumulated results and RNG seeds
  -> Acc (Matrix (Color, Word32)) -- ^ New results and new RNG seeds
render screen camera acc =
  zipWith (\(T2 new seed) (T2 old _) -> T2 (new + old) seed) result acc
  where
    rays = primaryRays (the camera) screen
    seeds = map snd acc
    result = map (traceRay 15 mainScene) $ zip rays seeds

-- | Calculate the origin and directions of the primary rays based on a camera
-- and a matrix of screen pixel positions. These positions should be in the
-- format @V2 <0 .. screenWidth> <0 .. screenHeight>@.
primaryRays ::
     Exp Camera -> Acc (Matrix (V2 Int)) -> Acc (Matrix RayF)
primaryRays ~(Camera_ cPos cRot (fromIntegral -> cFov)) = map transform
  where
    -- | The distance between the camera and the virtual screen plane
    screenDistance :: Exp Float
    screenDistance = 1.0 / tan((cFov * pi / 180.0) / 2.0 )

    -- | The looking direciton of the camera.
    cDir :: Exp Direction
    cDir = anglesToDirection cRot

    -- | The coordinates of a virtual screen plane. Instead of using regular
    -- matrix transformations, we'll simply map every pixel on the screen to a
    -- point on this virtual plane. We can then simply calculate the ray's
    -- direction by drawing a line between the camera's origin and the point
    -- we've calculated.
    planeCenter, planeTopOffset, planeRightOffset :: Exp (V3 Float)
    (planeCenter, planeTopOffset, planeRightOffset) =
      let center = cPos + cDir ^* screenDistance
          centerOffset = normalize $ center - cPos
          rightOffset = centerOffset `cross` constant upVector
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
          V2_ screenX screenY = rasterPos / screenSize * 2.0 + V2_ (-1.0) 1.0

          virtualPoint :: Exp Point
          virtualPoint = planeCenter + (planeRightOffset ^* screenX) + (planeTopOffset ^* screenY)
          rayDir :: Exp (V3 Float)
          rayDir = normalize $ virtualPoint - cPos
       in Ray_ cPos rayDir

-- | Calculate the amount of light that a given ray would collect when shot into
-- the scene. In other words, calculate what color the pixel that corresponds to
-- the ray should be.
--
-- Because if the way Accelerate is structured, we had to invert the recursive
-- flow. Instead of returning @emittance + (brdf * traceRay (limit - 1) scene
-- nextRay)@, we return @old_result + (emittance * throughput)@, where
-- throughput is a comulative product of the BRDFs, probability density
-- functions and material colors.
--
-- TODO: Make the emission, BRDF, and next ray direction functions of
--       'Material'.
-- TODO: Test if there's any performance loss when using 'while' instead of
--       'iterate'. This whould allow us to use russian roulette to reduce the
--       algorithm's bias.
-- TODO: I feel like some things are normalized when they shouldn't be. Diffuse
--       spheres look like they have a ring of bright light on them, but I feel
--       like it stops too abruptly for it to just be diffusion.
traceRay :: Exp Int -> Scene -> Exp (RayF, Word32) -> Exp (Color, Word32)
traceRay limit scene primaryRay =
  let T3 (T2 _ seed) result _ = iterate limit check (T3 primaryRay initialColor initialThroughput)
   in T2 result seed
  where
    initialColor, initialThroughput :: Exp (V3 Float)
    initialColor = V3_ 0.0 0.0 0.0
    initialThroughput = V3_ 1.0 1.0 1.0

    -- | Check whether the ray has hit something and calculate the prepare the
    -- next ray if we did.
    check ::
         Exp ((RayF, Word32), Color, V3 Float)
      -> Exp ((RayF, Word32), Color, V3 Float)
    check current@(T3 (T2 ray seed) result throughput) =
      let nextHit = closestIntersection scene ray
       in if nearZero throughput || isNothing nextHit
            then T3 (T2 ray seed) result (V3_ 0.0 0.0 0.0)
            else calculate current $ fromJust nextHit

    -- | Calculate the currently accumulated color and throughput as well as the
    -- next ray after we have intersected with something.
    calculate ::
         Exp ((RayF, Word32), Color, V3 Float)
      -> Exp (Normal, Material)
      -> Exp ((RayF, Word32), Color, V3 Float)
    calculate (T3 (T2 ray seed) result throughput)
              (T2 ~(Ray_ intersection iNormal) iMaterial) =
      let T2 rotationVector nextSeed = genVec seed

          nextRayProb = 1 / (pi * 2)
          mColor = iMaterial ^. color
          emittance = mColor ^* (iMaterial ^. illuminance)
          T2 nextDirection brdf' =
            caseof
              (iMaterial ^. brdf)
              -- Diffuse objects are modeled through Lambartian reflectance. The
              -- next ray should be fired somewhere in the hemisphere of the
              -- intersected primitives' normal.
              [ ( isDiffuse
                , let Brdf' _ p = iMaterial ^. brdf
                      next = rotate (anglesToQuaternion $ pi *^ rotationVector) iNormal
                      b = p / pi * (next `dot` iNormal)
                   in T2 next b)
              -- The glossy model uses the Blinn-Phong reflection model.
              , ( isGlossy
                  -- TODO: Find out what the correct way to scale the
                  --       'rotationVector' is. Right now I've chosen it in such
                  --       a way that @p = 0@ results in mirror-like behaviour.
                , let Brdf' _ p = iMaterial ^. brdf
                      intersectionAngle = (ray ^. direction) `dot` iNormal
                      reflection = (ray ^. direction) - 2 * intersectionAngle *^ iNormal
                      next = rotate (anglesToQuaternion $ (1 - p) *^ rotationVector) reflection
                      -- This has to be clamped to 0 as 'next' may be pointing
                      -- in to the area behind the intersection
                      b = max 0 $ p * (next `dot` reflection)
                   in T2 next b)
              ]
              (T2 (V3_ 0.0 0.0 0.0) 0)

          nextRay = Ray_ (intersection + nextDirection ^* epsilon) nextDirection
       in T3
            (T2 nextRay nextSeed)
            (result + (emittance * throughput))
            (throughput * mColor ^* (brdf' * nextRayProb))

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
