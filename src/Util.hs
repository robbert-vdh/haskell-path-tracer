{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}

-- | Small utility functions for conversions.

module Util where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Functor as A
import Data.Array.Accelerate.Linear as A
import qualified System.Random.MWC as Rng

import qualified Data.List as P
import Prelude (return, IO)
import qualified Prelude as P

import Intersection
import Scene.Objects
import Scene.World (getStartCamera)

-- * Functions

-- | Transform homogeneous coordinates back into regular vectors. There is a
-- function in 'Linear.V4' that has the same signature, but it also normalizes
-- based on the @w@ coordinate causing vectors (with @w = 0@) to become NaN.
fromHomogeneous :: Elt a => Exp (V4 a) -> Exp (V3 a)
fromHomogeneous ~(V4' x y z _) = V3' x y z

-- | Convert an integer vector to a float vector. This is only used when
-- converting between rasterization and world spaces.
vecToFloat ::
     (Functor f, Elt (f Int), Elt (f Float)) => Exp (f Int) -> Exp (f Float)
vecToFloat = fmap toFloating

-- | Use a linear congruential pseudo random number generator to generate a
-- 'Float' from a seed. The result consists of the generated float and a new
-- seed. The generated floats are in the `[-1, 1]` range.
genFloat :: Exp Word32 -> Exp (Float, Word32)
genFloat seed = T2 nextFloat nextSeed
  where
    modulus = 2 ^ (32 :: Exp Word32)
    multiplier = 1664525
    increment = 1013904223
    nextSeed = ((multiplier * seed) + increment) `mod` modulus

    nextFloat = fromIntegral nextSeed / (fromIntegral modulus / 2) - 1

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
--
-- TODO: Check whether there's a performance benefit to storing the primitives
--       as 'Exp''s instead of using 'constant' here.
mapScene ::
     Elt a
  => (forall p. Primitive p =>
                  Exp p -> Exp a)
  -> Scene
  -> [Exp a]
mapScene f (Scene s p) = P.map (f . constant) s P.++ P.map (f . constant) p

-- TODO: Think of a better naming scheme for these functions over @[Exp a]@

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
expMinWith _ [] = error "Invalid call to 'expMinWith'"
expMinWith _ [x] = x
expMinWith f (x:xs) =
  fst $
  P.foldl'
    (\a@(T2 _ valA) (calcKey -> b@(T2 _ valB)) -> cond (valA <= valB) a b)
    (calcKey x)
    xs
  where
    calcKey a = T2 a (f a)


-- * Definitions

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

-- | The output matrix initialized with all zero values and intiial seeds. This
-- is used during the initialization and after moving the camera.
--
-- TODO: Reseed the RNG after a certain number of iterations
initialOutput :: IO (A.Matrix (Color, Word32))
initialOutput = do
  rng <- Rng.create
  fromFunctionM screenShape $ \_ -> do
    seed <- Rng.uniform rng
    return (V3 0.0 0.0 0.0, seed)

-- | A matrix containing coordinates for every pixel on the screen. This is used
-- to cast the actual rays.
screenPixels :: (A.Matrix (V2 Int))
screenPixels = A.fromFunction screenShape $ \(Z :. y :. x) -> V2 x y

-- | The size of the output as an array shape.
screenShape :: Z :. Int :. Int
screenShape = Z :. P.fromIntegral screenHeight :. P.fromIntegral screenWidth

-- TODO: Replace this with some actual value
theCamera :: A.Exp Camera
theCamera = A.constant getStartCamera
