{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
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

-- | A marker for which ray tracing implementation to use. At the moment there
-- are two algorithms:
--
--    * A stream based algorithm, selectable with the '@streams@' option. This
--      variant is more advanced, modular, and allows features that require
--      diverging rays like light refraction to be implemented. The downside of
--      this approach is that most of the time is spent on spinlocks since
--      Accelerate's array fusion is not yet able to fuse the substeps in this
--      algorithm.
--    * The '@inline@' algorithm maps every pixel to a color and then adds those
--      colors to the old values. Since this approach has a 1-to-1 mapping
--      between pixels and rays features such as refraction cannot be
--      implemented here, but it does give a good indication as to how fast the
--      algorithm would be with ideal array fusion. As such this algorithm is
--      much faster than the stream based implementation.
--
-- TODO: Make the command line option use lowercase names and have it print out
--       the available options like clap would.
data Algorithm = Streams | Inline deriving (P.Read, P.Show)

-- instance Read Algorithm where

-- | The maximum number of bounces a ray can make.
maxIterations :: Exp Int
maxIterations = 15

-- | Render a single sample, combining the previous results with the newly
-- generated sample. In the application, we use the 'compileFor' function to fix
-- the first two arguments since those won't ever change until we have to reset
-- the computations because the camera was moved.
--
-- You can change the algorithms by passing a different variant of 'Algorithm'.
-- See below for more detailed explanations of the algorithms.
--
-- Because if the way Accelerate is structured, the recurrent formulation of the
-- resulting color values has to be inverted. Instead of returning @emittance +
-- (brdf * next_step_emittance@, we do @old_result + (emittance * throughput)@
-- at every step, where throughput is a comulative product of the BRDFs,
-- probability density functions and material colors. Essentially we're just
-- rewriting the typical head recursive formulation to tail recursion so we can
-- use regular loops instead.
--
-- * 'Streams'
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
--        accumulated results from the previous frame. To prevent having to
--        concatenate a bunch of vectors we'll do this as part of the iteration
--        cycle.
--
-- * 'Inline'
--
-- This approach is much simpler, and maps every pixel to a color value as part
-- of a single map operation. This does mean that some effects that require rays
-- to split into multiple rays, such as refraction, are not possible. Because
-- everything happens inside of a single map operation this approach is much
-- faster than the first because there's much less communication involved and
-- all ray tracing operations are fused into a single kernel.
render
  :: Algorithm
  -> Acc (Matrix (V2 Int)) -- ^ Screen pixel coordinates
  -> Acc (Scalar Camera)
  -> Acc RenderResult -- ^ Accumulated results and RNG seeds
  -> Acc RenderResult -- ^ New results and new RNG seeds
render Streams screen camera acc =
  let T3 _ finalResults _ = awhile
        notFinished
        (\(T3 state results iterations) ->
          let T2 state' newResults = traceStep mainScene state
              results'             = combine results newResults
              iterations'          = map succ iterations
          in  T3 state' results' iterations'
        )
        (T3 initialState acc (unit (0 :: Exp Int)))
  in  map updateSeed finalResults
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

  -- | We stop iterating after there are no more rays or if we reach the
  -- iterations limit, whichever comes first.
  notFinished
    :: Acc (Vector RayState, RenderResult, Scalar Int) -> Acc (Scalar Bool)
  notFinished (T3 state _ iterations) = if not (null state)
    then unit True_
    else map (\n -> n <= maxIterations && not (null state)) iterations

  -- | Add the newly calculated color values to the previous results. We lose
  -- the pixel value here since we have to match 'acc', but we can just look
  -- them up again in the original 'finalResults' in the index mapping step.
  --
  -- Because there's no guarantee that seeds are unique we use `updateSeed`
  -- after the final iteration to update the RNG seeds for the next rendering
  -- cycle.
  combine :: Acc RenderResult -> Acc (Vector RayResult) -> Acc RenderResult
  combine results newResults = permute
    (\(T2 lColor seed) (T2 rColor _) -> T2 (lColor + rColor) seed)
    results
    (\idx -> let T3 (V2_ x y) _ _ = newResults ! idx in Just_ $ index2 y x)
    (map (\(T3 _ c seed) -> T2 c seed) newResults)

  -- Generate a new RNG seed. Used after the rendering loop since we have no
  -- guarantees that the seeds from the results matrix are unique.
  --
  -- TODO: Is ts there a way around this?
  updateSeed :: Exp (Color, Word32) -> Exp (Color, Word32)
  updateSeed (T2 c seed) = T2 c (P.snd $ genFloat' seed)

render Inline screen camera acc = zipWith
  (\(T2 new seed') (T2 old _) -> T2 (new + old) seed')
  result
  acc
 where
  rays   = primaryRays (the camera) screen
  seeds  = map snd acc
  result = map (traceInline 15 mainScene) $ zip rays seeds

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
      -- FIXME: On the GPU backend this use of expand seems to a) cause the
      --        render to stall at a certain moment and b) adds some kind of
      --        noise that's not present on the CPU version. This is probably a
      --        bug in accelerate-llvm-ptx.
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
    let T2 iNormal iMaterial           = fromJust intersection
        -- TODO: When adding diverging rays, we should make sure to change the
        --       seed here
        T3 nextRay throughputMod seed' = calcNextRay iMaterial iNormal ray seed
    in  T4 nextRay pixel (throughput * throughputMod) seed'

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

-- | Calculate the amount of light that a given ray would collect when shot into
-- the scene. In other words, calculate what color the pixel that corresponds to
-- the ray should be. This performs the entire ray tracing algorithm in a single
-- step, with the downside that a ray can only produce at most one new ray.
--
-- TODO: Test if there's any performance loss when using 'while' instead of
--       'iterate'. This whould allow us to use russian roulette to reduce the
--       algorithm's bias.
-- TODO: I feel like some things are normalized when they shouldn't be. Diffuse
--       spheres look like they have a ring of bright light on them, but I feel
--       like it stops too abruptly for it to just be diffusion.
traceInline :: Exp Int -> Scene -> Exp (RayF, Word32) -> Exp (Color, Word32)
traceInline limit scene primaryRay =
  let T3 (T2 _ seed) result _ = iterate
        limit
        prepareRay
        (T3 primaryRay initialColor initialThroughput)
  in  T2 result seed
 where
  initialColor, initialThroughput :: Exp (V3 Float)
  initialColor      = V3_ 0.0 0.0 0.0
  initialThroughput = V3_ 1.0 1.0 1.0

  -- | Check whether the ray has hit something. If it did, call `computeRay` to
  -- calculate the resulting color value and to prepare the next ray after
  -- bouncing.
  prepareRay
    :: Exp ((RayF, Word32), Color, V3 Float)
    -> Exp ((RayF, Word32), Color, V3 Float)
  prepareRay current@(T3 (T2 ray seed) result throughput) =
    let nextHit = checkHit scene ray
    in  if nearZero throughput || isNothing nextHit
          then T3 (T2 ray seed) result (V3_ 0.0 0.0 0.0)
          else computeRay current $ fromJust nextHit

  -- | Calculate the currently accumulated color and throughput as well as the
  -- next ray after we have intersected with something.
  computeRay
    :: Exp ((RayF, Word32), Color, V3 Float)
    -> Exp (NormalP, Material)
    -> Exp ((RayF, Word32), Color, V3 Float)
  computeRay (T3 (T2 ray seed) result throughput) (T2 iNormal iMaterial) =
    let
      -- TODO: Extract this to a function as it's the same as in 'tracestep'
        mColor                         = iMaterial ^. color
        emittance                      = mColor ^* (iMaterial ^. illuminance)

        T3 nextRay throughputMod seed' = calcNextRay iMaterial iNormal ray seed
    in  T3 (T2 nextRay seed')
           (result + (emittance * throughput))
           (throughput * throughputMod)

-- | Calculate a next ray and throughput modifier based on an intersected
-- primitive's material, the intersection point, the normal at the intersection
-- and the previous ray.
--
-- The resulting value is a pair of @(next_ray, throughput_modifier, next_seed)@. The
-- throughput modifier has to be multiplied with the old throughput to obtain
-- the new throughput values.
calcNextRay
  :: Exp Material
  -> Exp NormalP
  -> Exp RayF
  -> Exp Word32
  -> Exp (RayF, V3 Float, Word32)
calcNextRay iMaterial (Ray_ iPoint iNormal) ray seed =
  let
    T2 rotationVector seed' = genVec seed
    T2 nextDirection  brdf' = (iMaterial ^. brdf) & match \case
      -- Diffuse objects are modeled through Lambartian reflectance. The
      -- next ray should be fired somewhere in the hemisphere of the
      -- intersected primitives' normal.
      Diffuse_ p ->
        let next = rotate (anglesToQuaternion $ pi *^ rotationVector) iNormal
            b    = p / pi * (next `dot` iNormal)
        in  T2 next b
      -- The glossy model uses the Blinn-Phong reflection model.
      -- TODO: Find out what the correct way to scale the
      --       'rotationVector' is. Right now I've chosen it in such a
      --       way that @p = 0@ results in mirror-like behaviour.
      Glossy_ p ->
        let
          intersectionAngle = (ray ^. direction) `dot` iNormal
          reflection = (ray ^. direction) - 2 * intersectionAngle *^ iNormal
          next =
            rotate (anglesToQuaternion $ (1 - p) *^ rotationVector) reflection
          -- This has to be clamped to 0 as 'next' may be pointing in to
          -- the area behind the intersection
          b = max 0 $ p * (next `dot` reflection)
        in
          T2 next b

    nextRay     = Ray_ (iPoint + nextDirection ^* epsilon) nextDirection
    nextRayProb = 1 / (pi * 2)
    mColor      = iMaterial ^. color
  in
    T3 nextRay (mColor ^* (brdf' * nextRayProb)) seed'

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
