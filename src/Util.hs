{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Small utility functions for conversions.
module Util where

import           Control.Monad                  ( replicateM )
import           Control.Monad.State.Strict     ( runState
                                                , state
                                                )
import           Data.Array.Accelerate         as A
                                         hiding ( pattern V2
                                                , pattern V3
                                                )
import           Data.Array.Accelerate.Data.Bits
                                                ( unsafeShiftL
                                                , unsafeShiftR
                                                , xor
                                                )
import qualified Data.Array.Accelerate.Sugar.Shape
                                               as S
import           Data.Array.Accelerate.Control.Lens
import           Data.Array.Accelerate.Data.Functor
                                               as A
                                         hiding ( (<$>) )
import           Data.Array.Accelerate.Linear  as A
import qualified System.Random.MWC             as Rng
import qualified Linear                        as L

import qualified Data.List                     as P
import           Prelude                        ( (<$>)
                                                , (<*>)
                                                , IO
                                                )
import qualified Prelude                       as P

import           Scene.Intersection
import           Scene.Objects

-- * Functions
-- ** Accelerate

-- | Wrap a value in a accelerate array. This is equivalent to 'unit', but not
-- lifted to 'Exp' and 'Acc'.
scalar :: Elt a => a -> Scalar a
scalar x = fromList Z [x]

-- ** Vector operations

-- | Convert the euler angles stored in the 'Camera' to a looking direction
anglesToDirection :: Exp Direction -> Exp Direction
anglesToDirection angles =
  rotate (anglesToQuaternion angles) $ constant forwardVector

-- | Convert an unnormalized euler axis rotation vector into a 'Quaternion'. The
-- conversion was copied from
-- https://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles#Source_Code.
anglesToQuaternion :: Exp Direction -> Exp (Quaternion Float)
anglesToQuaternion (V3_ roll pitch yaw) =
  Quaternion_ (cy' * cp' * cr' + sy' * sp' * sr') $ V3_
    (cy' * cp' * sr' - sy' * sp' * cr')
    (sy' * cp' * sr' + cy' * sp' * cr')
    (sy' * cp' * cr' - cy' * sp' * sr')
 where
  cy' = cos $ yaw * 0.5
  sy' = sin $ yaw * 0.5
  cp' = cos $ pitch * 0.5
  sp' = sin $ pitch * 0.5
  cr' = cos $ roll * 0.5
  sr' = sin $ roll * 0.5

-- | Convert an unnormalized euler axis rotation vector into a 'Quaternion'.
anglesToQuaternion' :: Direction -> Quaternion Float
anglesToQuaternion' (V3 roll pitch yaw) =
  Quaternion (cy * cp * cr + sy * sp * sr) $ V3 (cy * cp * sr - sy * sp * cr)
                                                (sy * cp * sr + cy * sp * cr)
                                                (sy * cp * cr - cy * sp * sr)
 where
  cy = cos $ yaw * 0.5
  sy = sin $ yaw * 0.5
  cp = cos $ pitch * 0.5
  sp = sin $ pitch * 0.5
  cr = cos $ roll * 0.5
  sr = sin $ roll * 0.5

translate :: Direction -> Camera -> Camera
translate delta camera = camera & position' +~ translation
 where
  translation = L.rotate (anglesToQuaternion' $ camera ^. rotation') delta

-- | Transform homogeneous coordinates back into regular vectors. There is a
-- function in 'Linear.V4' that has the same signature, but it also normalizes
-- based on the @w@ coordinate causing vectors (with @w = 0@) to become NaN.
fromHomogeneous :: Elt a => Exp (V4 a) -> Exp (V3 a)
fromHomogeneous (V4_ x y z _) = V3_ x y z

-- | The world's 'forward' direction. This is the direction the camera looks
-- into when it has not been rotated.
forwardVector :: V3 Float
forwardVector = V3 0.0 0.0 (-1.0)

-- | The world's 'up' direction. This is used in combination in cross products
-- to calculate orthogonal vectors and perspectives.
upVector :: V3 Float
upVector = V3 0.0 1.0 0.0

-- | Convert an integer vector to a float vector. This is only used when
-- converting between rasterization and world spaces.
vecToFloat
  :: (Functor f, Elt (f Int), Elt (f Float)) => Exp (f Int) -> Exp (f Float)
vecToFloat = fmap toFloating

-- ** RNG

-- | Use an xorshift pseudo random number generator to generate a 'Float' from a
-- seed. The result consists of the generated float and a new seed. The
-- generated floats are in the `[-1, 1]` range.
--
-- The function returns a tuple of 'Exp's instead of a single 'Exp' tuple so we
-- can make use of the state monad.
genFloat :: Exp Word32 -> Exp (Float, Word32)
genFloat = P.uncurry T2 . genFloat'

-- | The same function as 'genFloat', but with the result and the next seed
-- split into a tuple of 'Exp's instead of a single 'Exp' tuple. This is so we
-- can use the state monad to thread the seed to multiple calculations.
genFloat' :: Exp Word32 -> (Exp Float, Exp Word32)
genFloat' seed = (nextFloat, nextSeed)
 where
  seed'     = seed `xor` (seed `unsafeShiftL` 13)
  seed''    = seed' `xor` (seed' `unsafeShiftR` 17)
  nextSeed  = seed'' `xor` (seed'' `unsafeShiftL` 5)

  nextFloat = fromIntegral nextSeed / (2 ** 31) - 1

-- | Generate a random vector whose three values are in the range @[-1, 1]@.
-- This value can be used to create a quaternion for rotation a vector.
genVec :: Exp Word32 -> Exp (V3 Float, Word32)
genVec seed = P.uncurry T2 $ runState (V3_ <$> rng <*> rng <*> rng) seed
  where rng = state genFloat'

-- | Create a RNG seed for every screen pixel. This function is used when
-- resetting the rendering texture and when reseeding the RNGs.
genSeeds :: IO [Word32]
genSeeds = do
  rng <- Rng.createSystemRandom
  replicateM (S.size screenShape) (Rng.uniform rng)

-- | Reseed a rendering texture by replacing the seeds stored in every pixel
-- tuple with a newly generated value. This is important because 'genFloat'
-- relies on a very simple PRNG. While this is good enough for our use cases,
-- you can still start to see the RNGs converge if you leave the algorithm
-- running for long enough.
--
-- TODO: Use Vectors instead of lists here
reseed :: RenderResult -> IO RenderResult
reseed m =
  fromList screenShape . P.zipWith (\(c, _) s -> (c, s)) old <$> genSeeds
  where old = toList m

-- ** Mapping and folding over a scene
--
-- Accelerate does not support sum types (yet), but we still want a nice and
-- convenient way to operate on and compare every object in a scene. Our
-- solution to this problem is simply mapping every primitive @Exp p@ to an @Exp
-- a@ and storing those results in a list. Ideally we would then lift that list
-- to an @Exp [a]@ so we can fold and map over that list, but that's not an
-- option because of 'Exp''s semantics. Using Accelerate's array structures
-- instead of lists is also not possible since any operation on those arrays
-- should be executed in an 'Acc' context, and we're already doing calculations
-- on 'Exp' level here.
--
-- Here we simply abuse the fact that our scene is static accross the whole
-- runtime of the application. Even though we're mapping and folding over lists,
-- the compiler can simply unfold these loops during compilation. That allows us
-- to still use higher order functions while not making things more complicated
-- than they should be.

-- | Map a function over every object in a scene.
mapScene
  :: Elt a => (forall p . Primitive p => Exp p -> Exp a) -> Scene -> [Exp a]
mapScene f (Scene s p) = P.map f s P.++ P.map f p

-- | Determines whether the predicate returns 'True' for any value in the list.
expAny :: (Exp a -> Exp Bool) -> [Exp a] -> Exp Bool
expAny f = P.foldr (\x acc -> acc || f x) (constant False)

-- | Find the smallest value in a list of 'Exp a'.
expMin :: Ord a => [Exp a] -> Exp a
expMin = expMinWith P.id

-- | Find the smallest value in a list of 'Exp a' by applying a function. The
-- definition here is a bit ugly, but I was not sure whether Haskell's laziness
-- would transfer over to the comopiled program.
expMinWith :: (Ord b, Elt a) => (Exp a -> Exp b) -> [Exp a] -> Exp a
expMinWith _ []       = error "Invalid call to 'expMinWith'"
expMinWith _ [x     ] = x
expMinWith f (x : xs) = fst $ P.foldl'
  (\a@(T2 _ valA) (calcKey -> b@(T2 _ valB)) -> cond (valA <= valB) a b)
  (calcKey x)
  xs
  where calcKey a = T2 a (f a)

-- * Definitions

-- | The dimensions of the screen. These are hard coded for efficiency's sake
-- even though the window could be resizable with minor adjustments.
--
-- TODO: Make this configurable through a command line option
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
  V2_ (P.fromIntegral screenWidth) (P.fromIntegral $ negate screenHeight)

-- | The output matrix initialized with all zero values and intiial seeds. This
-- is used during the initialization and after moving the camera.
initialOutput :: IO (Matrix (Color, Word32))
initialOutput = fromList screenShape . P.map (V3 0.0 0.0 0.0, ) <$> genSeeds

-- | A matrix containing coordinates for every pixel on the screen. This is used
-- to cast the actual rays.
screenPixels :: (Matrix (V2 Int))
screenPixels = fromFunction screenShape $ \(Z :. y :. x) -> V2 x y

-- | The size of the output as an array shape.
screenShape :: Z :. Int :. Int
screenShape = Z :. P.fromIntegral screenHeight :. P.fromIntegral screenWidth
