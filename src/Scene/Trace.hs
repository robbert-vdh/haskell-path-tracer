{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeApplications #-}
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

import           Data.Array.Accelerate
import           Data.Array.Accelerate.Control.Lens
                                         hiding ( transform
                                                , use
                                                )
import           Data.Array.Accelerate.Data.Maybe
import           Data.Array.Accelerate.Data.Functor
import           Data.Array.Accelerate.Linear

import qualified Prelude                       as P
                                                ( )

import           Scene.Intersection
import           Scene.Objects
import           Scene.World
import           Util

-- | A ray along with all the information needed to produce color values. The
-- format here is:
--
-- @
--     (ray, screen_pixel, rng_seed, throughput)
-- @
--
type RayState = (RayF, V2 Int, V3 Float, Word32)

-- | The resulting color contribution from a ray hitting a primitive. This is a
-- pair of @(screen_pixel, color_value, seed)@. All of these generated values
-- should be added to the accumulated value from the previous frame during the
-- rendering cycle.
type RayResult = (V2 Int, Color, Word32)

-- | The maximum number of bounces a ray can make.
maxIterations :: Exp Int
maxIterations = 15

-- | Render a single sample, combining the previous results with the newly
-- generated sample. In the application, we use the 'compileFor' function to fix
-- the first two arguments since those won't ever change until we have to reset
-- the computations because the camera was moved.
--
-- A single rendering step consists of the following substeps:
--
--     1. Create primary rays for every pixel on the screen based on the camera
--        configuration. For every ray we also store a throughput value which
--        starts at 1.0 and gradually decreases as the rays bounce around.
--
--     2. For every ray, determine the first primitive it intersects with, if
--        any.
--     3. If there was an intersection, compute the primitive's contribution to
--        the screen pixel's color using the primitive's material and the
--        current throughput value. Depending on whether the ray hit and the
--        primitive's material, the ray will expand into zero, one, or two new
--        rays. For every new ray we store will store the ray itself, the screen
--        pixel coordinate corresponding to the ray, a new RNG seed and the new
--        throughput value depending on how the light bounced.
--
--        TODO: Actually implement materials that can produce multiple rays,
--              such as glass.
--
--     4. Steps 2 and 3 get repeated until we either reach a maximum number of
--        iterations or there are no new rays being produced.
--     5. Finally we add the produced color values for each pixel to the
--        accumulated results from the previous frame.
--
--        TODO: If accelerate can parallelize these kernels properly we can do
--              this summing step during step 3 so we don't have to do it all at
--              the end. That would also avoid having to concatenate a lot of
--              arrays.
--
-- Because if the way Accelerate is structured, the recurrent formulation of the
-- resulting colors had to be inverted. Instead of returning @emittance + (brdf
-- * traceRay (limit - 1) scene nextRay)@, we return @old_result + (emittance *
-- throughput)@, where throughput is a comulative product of the BRDFs,
-- probability density functions and material colors.
--
-- TODO: Reword the above paragraph with updated function and value names
render
  :: Acc (Matrix (V2 Int)) -- ^ Screen pixel coordinates
  -> Acc (Scalar Camera)
  -> Acc RenderResult -- ^ Accumulated results and RNG seeds
  -> Acc RenderResult -- ^ New results and new RNG seeds
render screen camera acc =
  let T3 _ finalResults _ = awhile
        notFinished
        (\(T3 state results iterations) ->
          let T2 state' newResults = traceStep mainScene state
              results'             = results ++ newResults
              iterations'          = map succ iterations
          in  T3 state' results' iterations'
        )
        (T3 initialState initialResults (unit (0 :: Exp Int)))
  in
      -- We just add all the resulting pixel values to the accumulated values
      -- from the previous frames. We lose the pixel value here since we have to
      -- match 'acc', but we can just look them up again in the original
      -- 'finalResults' in the index mapping step.
      -- TODO: Probably use xor for combining the seeds
      permute
        (\(T2 lColor lSeed) (T2 rColor rSeed) ->
          T2 (lColor + rColor) (lSeed + rSeed)
        )
        acc
        (\idx -> let T3 (V2_ x y) _ _ = finalResults ! idx in index2 y x)
        (map (\(T3 _ c seed) -> T2 c seed) finalResults)
 where
  -- TODO: Can we avoid this @map snd@ by using a tuple? We would probably need
  --       to do two permutes in that case.
  initialSeeds      = map snd acc
  initialRays       = primaryRays (the camera) screen
  initialThroughput = V3_ @Float 1.0 1.0 1.0
  initialState      = flatten $ zipWith3
    (\ray pixel seed -> T4 ray pixel initialThroughput seed)
    initialRays
    screen
    initialSeeds
  initialResults = use (fromList (Z :. 0) [])

  -- | We stop iterating after there are no more rays or if we reach the
  -- iterations limit, whichever comes first.
  --
  -- TODO: Can we use 'null' this way?
  notFinished
    :: Acc (Vector RayState, Vector RayResult, Scalar Int) -> Acc (Scalar Bool)
  notFinished (T3 state _ iterations) = if not (null state)
    then unit True_
    else map (\n -> n <= maxIterations && not (null state)) iterations

-- | Calculate the origin and directions of the primary rays based on a camera
-- and a matrix of screen pixel positions. These positions should be in the
-- format @V2 <0 .. screenWidth> <0 .. screenHeight>@.
primaryRays :: Exp Camera -> Acc (Matrix (V2 Int)) -> Acc (Matrix RayF)
primaryRays (Camera_ cPos cRot (fromIntegral -> cFov)) = map transform
 where
  -- | The distance between the camera and the virtual screen plane
  screenDistance :: Exp Float
  screenDistance = 1.0 / tan ((cFov * pi / 180.0) / 2.0)

  -- | The looking direciton of the camera.
  cDir :: Exp Direction
  cDir = anglesToDirection cRot

  -- | The coordinates of a virtual screen plane. Instead of using regular
  -- matrix transformations, we'll simply map every pixel on the screen to a
  -- point on this virtual plane. We can then simply calculate the ray's
  -- direction by drawing a line between the camera's origin and the point we've
  -- calculated.
  planeCenter, planeTopOffset, planeRightOffset :: Exp (V3 Float)
  (planeCenter, planeTopOffset, planeRightOffset) =
    let center       = cPos + cDir ^* screenDistance
        centerOffset = normalize $ center - cPos
        rightOffset  = centerOffset `cross` constant upVector
        topOffset    = (cDir `cross` rightOffset) ^/ screenAspect
    in  (center, topOffset, rightOffset)

  transform :: Exp (V2 Int) -> Exp RayF
  transform (vecToFloat -> rasterPos) =
    let
      -- Screen space is the space where both X and Y coordinates lie within the
      -- @[-1, 1]@ interval. Here @(-1, -1)@ is the bottom left and @(1, 1)@ is
      -- the top right corner. Because of this the @y@ axis has to be inverted
      -- during the computation. This has already been accounted for in
      -- 'screenSize', hence why the Y-axis value gets increased by two.
      V2_ screenX screenY = rasterPos / screenSize * 2.0 + V2_ (-1.0) 1.0

      virtualPoint :: Exp Point
      virtualPoint =
        planeCenter
          + (planeRightOffset ^* screenX)
          + (planeTopOffset ^* screenY)
      rayDir :: Exp (V3 Float)
      rayDir = normalize $ virtualPoint - cPos
    in
      Ray_ cPos rayDir

-- | Perform a single step of the ray tracing algorithm as described in
-- 'render'. The resulting vectors contain new rays for the next step of the
-- algorithm and color values produced by this step's rays.
--
-- TODO: Make the emission, BRDF, and next ray direction functions of
--       'Material'.
-- TODO: I feel like some things are normalized when they shouldn't be. Diffuse
--       spheres look like they have a ring of bright light on them, but I feel
--       like it stops too abruptly for it to just be diffusion.
-- TODO: It's still a lot slower than the old approach
traceStep
  :: Scene -> Acc (Vector RayState) -> Acc (Vector RayState, Vector RayResult)
traceStep scene state =
  let intersections = map
        (\(T4 ray pixel throughput seed) ->
          T5 ray (checkHit scene ray) pixel throughput seed
        )
        state
      newState = expand
        (\(T5 _ intersection _ throughput _) ->
          numNewRays intersection throughput
        )
        computeNewRay
        intersections
      results = expand
        (\(T5 _ intersection _ _ _) -> maybe 0 (const 1) intersection)
        computeResult
        intersections
  in  T2 newState results
 where
  -- | Compute a new ray after intersecting a primitive. This is to be used with
  -- 'expand' and assumes that there has been an intersection. Right now we
  -- haven't implemented any features that would cause rays to diverge, so we
  -- can ignore the integer argument for now.
  computeNewRay
    :: Exp (RayF, Maybe (NormalP, Material), V2 Int, V3 Float, Word32)
    -> Exp Int
    -> Exp RayState
  computeNewRay (T5 ray intersection pixel throughput seed) _ =
    let
      T2 (Ray_ iPoint iNormal) iMaterial = fromJust intersection
      -- TODO: When adding diverging rays, we should make sure to change the
      --       seed here
      T2 rotationVector        seed'     = genVec seed
      -- TODO: Move brdf part to another function
      T2 nextDirection         brdf'     = caseof
        (iMaterial ^. brdf)
        [ ( isDiffuse
            -- Diffuse objects are modeled through Lambartian reflectance. The
            -- next ray should be fired somewhere in the hemisphere of the
            -- intersected primitives' normal.
          , let Brdf_ _ p = iMaterial ^. brdf
                next =
                  rotate (anglesToQuaternion $ pi *^ rotationVector) iNormal
                b = p / pi * (next `dot` iNormal)
            in  T2 next b
          )
        , ( isGlossy
            -- The glossy model uses the Blinn-Phong reflection model.
            -- TODO: Find out what the correct way to scale the
            --       'rotationVector' is. Right now I've chosen it in such a
            --       way that @p = 0@ results in mirror-like behaviour.
          , let
              Brdf_ _ p         = iMaterial ^. brdf
              intersectionAngle = (ray ^. direction) `dot` iNormal
              reflection =
                (ray ^. direction) - 2 * intersectionAngle *^ iNormal
              next = rotate (anglesToQuaternion $ (1 - p) *^ rotationVector)
                            reflection
              -- This has to be clamped to 0 as 'next' may be pointing in to
              -- the area behind the intersection
              b = max 0 $ p * (next `dot` reflection)
            in
              T2 next b
          )
        ]
        (T2 (V3_ 0.0 0.0 0.0) 0)

      nextRay     = Ray_ (iPoint + nextDirection ^* epsilon) nextDirection
      nextRayProb = 1 / (pi * 2)
      mColor      = iMaterial ^. color
    in
      T4 nextRay pixel (throughput * mColor ^* (brdf' * nextRayProb)) seed'

  -- | Compute the 'RayResult' for a ray that intersects a primitive. This is
  -- to be used with 'expand' and assumes that there has been an intersection.
  computeResult
    :: Exp (RayF, Maybe (NormalP, Material), V2 Int, V3 Float, Word32)
    -> Exp Int
    -> Exp RayResult
  computeResult (T5 _ intersection pixel throughput seed) _ =
    let T2 _ iMaterial = fromJust intersection
        mColor         = iMaterial ^. color
        emittance      = mColor ^* (iMaterial ^. illuminance)
    in  T3 pixel (emittance * throughput) seed

-- | Compute the number of new rays resulting from an intersection based on the
-- material. This also takes the current throughput in account, and we'll
-- terminate a ray if its throughput is near zero.
--
-- TODO: Since we only support purely diffuse and specular materials at the
--       moment this only returns 0 or 1.
numNewRays :: Exp (Maybe (NormalP, Material)) -> Exp (V3 Float) -> Exp Int
numNewRays iMaterial throughput =
  if nearZero throughput || isNothing iMaterial then 0 else 1

-- | Check if a ray hits a primitive. If the ray did intersect a primitive,
-- return the intersection, the normal of the intersected primitive at that
-- intersection, and the intersected primitive's material.
--
-- TODO: Maybe find some nicer way to write this, it took me five minutes of
--       staring at this to understand again what I was doing here.
checkHit :: Scene -> Exp RayF -> Exp (Maybe (NormalP, Material))
checkHit scene ray = fmap snd $ expMinWith (maybe infinite fst) $ mapScene
  (\p -> pairHit p <$> distanceTo ray p)
  scene
  where pairHit p t = T2 t (hit ray t p)

-- | Max possible value for a float, usefull as distance if there is not hit.
infinite :: Exp Float
infinite = encodeFloat 16777215 104

-- | A small offset used to prevent rays from intersecting with the same object
-- it last intersected with.
epsilon :: Exp Float
epsilon = 0.002
