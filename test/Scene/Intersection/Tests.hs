module Scene.Intersection.Tests
  ( tests
  ) where

import qualified Data.Array.Accelerate as A
import Data.Array.Accelerate (constant, unit, Elt, Exp)
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Linear hiding (distance)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import qualified Linear as L

import Lib (run)
import Scene.Intersection
import Scene.Objects

tests :: TestTree
tests = testGroup "Scene.Intersection" [sphereTests, planeTests]

sphereTests :: TestTree
sphereTests =
  testGroup
    "Sphere"
    [ testProperty "intersection ((x, 0, x), x) = (0, 0, x)" $
      property $ do
        diameter <- forAll $ Gen.float (Range.linearFrac 0.0 100.0)
        let sphere = makeSphere (V3 diameter 0.0 diameter) diameter
            ray = Ray' (V3' 0.0 0.0 0.0) (V3' 0.0 0.0 1.0)
            distance = fromJust $ distanceTo ray sphere
            A.T2 (Ray' hitPos _) _ = hit ray distance sphere

        evalExp hitPos === V3 0.0 0.0 diameter
    , testProperty "distanceTo ((x, x, x), y) = ||y|| - y + ||(x - y)||" $
      property $ do
        diameter <- forAll $ Gen.float (Range.linearFrac 0.1 100.0)
        offset <- forAll $ Gen.float (Range.linearFrac 0.1 100.0)
        let pos = diameter + offset
            sphere = makeSphere (V3 pos pos pos) diameter
            ray = Ray' (V3' 0.0 0.0 0.0) (normalize $ V3' 1.0 1.0 1.0)
            distance = distanceTo ray sphere

        -- Floating point rounding makes this a bit harder than it should be, so
        -- we'll just round the results and asusme they are equal
        (roundTo 2 <$> evalExp distance) ===
          Just
            (roundTo 2 $
              sqrt (3 * (diameter ** 2)) - diameter +
              sqrt (3 * (offset ** 2)))
    , testProperty "backface culling" $
      property $ do
        diameter <- forAll $ Gen.float (Range.linearFrac 0.1 100.0)
        ray <-
          forAll $
          Ray' (V3' 0.0 0.0 0.0) . normalize . constant <$>
          v3 (Range.linearFrac (-1.0) 1.0)
        let sphere = makeSphere (V3 0.0 0.0 0.0) diameter

        -- | Hits to a sphere's backface should not be registered
        evalExp (distanceTo ray sphere) === Nothing
    ]

planeTests :: TestTree
planeTests = 
  testGroup
    "Plane"
    [
      testProperty "Continious Straight on" $
      property $ do
        x <- forAll $ Gen.int (Range.linearBounded :: Range Int)
        y <- forAll $ Gen.int (Range.linearBounded :: Range Int)
        z <- forAll $ Gen.float (Range.linearFrac 1.0 1000.0)
        let pos = V3 (fromIntegral x) (fromIntegral y) z
            nor = V3 0.0 0.0 (-1.0)
            plane = makePlane pos nor
            ray = Ray' (V3' 0.0 0.0 0.0) (V3' 0.0 0.0 1.0)

        -- Check if there was a hit
        evalExp (distanceTo ray plane) === Just z
    , testProperty "Backface culling Straight on" $
      property $ do
        x <- forAll $ Gen.int (Range.linearBounded :: Range Int)
        y <- forAll $ Gen.int (Range.linearBounded :: Range Int)
        z <- forAll $ Gen.float (Range.linearFrac 1.0 1000.0)
        let pos = V3 (fromIntegral x) (fromIntegral y) z
            nor = V3 0.0 0.0 1.0
            plane = makePlane pos nor
            ray = Ray' (V3' 0.0 0.0 0.0) (V3' 0.0 0.0 1.0)

        -- Check if there was a hit
        evalExp (distanceTo ray plane) === Nothing
    , testProperty "Continious Angles" $
      property $ do
        x <- forAll $ Gen.float (Range.linearFrac (-0.99) 0.99)
        y <- forAll $ Gen.float (Range.linearFrac (-0.99) 0.99)
        let dir = constant (V3 x y 1.0)
            ray = Ray' (V3' 0.0 0.0 0.0) dir
            plane = makePlane (V3 0.0 0.0 1.0) (V3 0.0 0.0 (-1.0))

        -- TODO: get angle the ray is at, use pythagoras to find distance.
        -- replace this asser with something more usefull
        evalExp (distanceTo ray plane) /== Nothing
    , testProperty "Backface culling Angles" $
      property $ do
        x <- forAll $ Gen.float (Range.linearFrac (-0.99) 0.99)
        y <- forAll $ Gen.float (Range.linearFrac (-0.99) 0.99)
        let dir = constant (V3 x y 1.0)
            ray = Ray' (V3' 0.0 0.0 0.0) dir
            plane = makePlane (V3 0.0 0.0 1.0) (V3 0.0 0.0 1.0)

        evalExp (distanceTo ray plane) === Nothing
    ]

    -- | Evaluate a single Accelerate expression to a value. This is needed because
-- we can't compare unevaluated expressions directly.
evalExp :: Elt a => Exp a -> a
evalExp e = head $ A.toList $ run (unit e)

makeSphere :: V3 Float -> Float -> Exp Sphere
makeSphere pos diameter =
  constant $
  Sphere
    { _spherePosition = pos
    , _sphereRadius = diameter
    , _sphereMaterial = dummyMaterial
    }

-- | Generate a dummy plane by its pos and normal
makePlane :: V3 Float -> V3 Float -> Exp Plane
makePlane pos nor =
  constant $
  Plane
    { _planeDirection = nor
    , _planePosition = pos
    , _planeMaterial = dummyMaterial
    }

-- | Get a dummy material to use usefull as placeholder.
dummyMaterial :: Material
dummyMaterial = Material
  { _materialColor = V3 1.0 1.0 1.0
  , _materialIlluminance = 1.0
  , _materialBrdf = Diffuse 1.0
  }


-- | Round a number to a certain number of places. Uesful in comparisons.
roundTo :: Int -> Float -> Float
roundTo places i = fromInteger (round $ i * (10 ^ places)) / (10.0 ^^ places)

-- * Generators

v3 :: Range Float -> Gen (V3 Float)
v3 r = V3 <$> Gen.float r <*> Gen.float r <*> Gen.float r
