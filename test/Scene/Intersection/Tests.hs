module Scene.Intersection.Tests
  ( tests
  )
where

import qualified Data.Array.Accelerate         as A
import           Data.Array.Accelerate          ( Elt
                                                , Exp
                                                , constant
                                                , unit
                                                )
import           Data.Array.Accelerate.Data.Maybe
import           Data.Array.Accelerate.Linear
                                         hiding ( distance
                                                , point
                                                )
import           Data.Array.Accelerate.LLVM.Native
                                                ( run )
import           Hedgehog
import qualified Hedgehog.Gen                  as Gen
import qualified Hedgehog.Range                as Range
import           Test.Tasty
import           Test.Tasty.Hedgehog
import qualified Linear                        as L

import           Scene.Intersection
import           Scene.Objects

tests :: TestTree
tests = testGroup "Scene.Intersection" [sphereTests, planeTests]

sphereTests :: TestTree
sphereTests = testGroup
  "Sphere"
  [ testProperty "intersection ((x, 0, x), x) = (0, 0, x)" $ property $ do
    diameter <- forAll $ Gen.float (Range.linearFrac 0.0 100.0)
    let sphere                 = makeSphere (V3 diameter 0.0 diameter) diameter
        ray                    = Ray_ (V3_ 0.0 0.0 0.0) (V3_ 0.0 0.0 1.0)
        distance               = fromJust $ distanceTo ray sphere
        A.T2 (Ray_ hitPos _) _ = hit ray distance sphere

    (roundTo 3 <$> evalExp hitPos) === V3 0.0 0.0 (roundTo 3 diameter)
  , testProperty "distanceTo ((x, x, x), y) = ||y|| - y + ||(x - y)||"
  $ property
  $ do
      diameter <- forAll $ Gen.float (Range.linearFrac 0.1 100.0)
      offset   <- forAll $ Gen.float (Range.linearFrac 0.1 100.0)
      let pos      = diameter + offset
          sphere   = makeSphere (V3 pos pos pos) diameter
          ray      = Ray_ (V3_ 0.0 0.0 0.0) (normalize $ V3_ 1.0 1.0 1.0)
          distance = distanceTo ray sphere

      -- Floating point rounding makes this a bit harder than it should be, so
      -- we'll just round the results and asusme they are equal
      (roundTo 1 <$> evalExp distance) === Just
        (roundTo 1 $ sqrt (3 * (diameter ** 2)) - diameter + sqrt
          (3 * (offset ** 2))
        )
  , testProperty "backface culling" $ property $ do
    diameter <- forAll $ Gen.float (Range.linearFrac 0.1 100.0)
    ray      <- forAll $ Ray_ (V3_ 0.0 0.0 0.0) . normalize . constant <$> v3
      (Range.linearFrac (-1.0) 1.0)
    let sphere = makeSphere (V3 0.0 0.0 0.0) diameter

    -- Hits to a sphere's backface should not be registered
    evalExp (distanceTo ray sphere) === Nothing
  , testProperty "no backwards intersections" $ property $ do
    rayDirection <- forAll $ L.normalize <$> v3 (Range.linearFrac (-1.0) 1.0)
    let ray    = Ray_ (V3_ 0.0 0.0 0.0) (constant rayDirection)
        sphere = makeSphere (negate rayDirection) 0.1

    evalExp (distanceTo ray sphere) === Nothing
  ]

planeTests :: TestTree
planeTests = testGroup
  "Plane"
  [ testProperty "Continuous Straight on" $ property $ do
    pos@(V3 _ _ z) <- forAll point
    let nor   = V3 0.0 0.0 (-1.0)
        plane = makePlane pos nor
        ray   = Ray_ (V3_ 0.0 0.0 0.0) (V3_ 0.0 0.0 1.0)

    -- Check if there was a hit
    evalExp (distanceTo ray plane) === if z >= 0.0 then Just z else Nothing
  , testProperty "Backface culling Straight on" $ property $ do
    pos <- forAll point
    let nor   = V3 0.0 0.0 1.0
        plane = makePlane pos nor
        ray   = Ray_ (V3_ 0.0 0.0 0.0) (V3_ 0.0 0.0 1.0)

    -- Check if there was a hit
    evalExp (distanceTo ray plane) === Nothing
  , testProperty "Continuous Angles" $ property $ do
    x <- forAll $ Gen.float (Range.linearFrac (-1000.0) 1000.0)
    y <- forAll $ Gen.float (Range.linearFrac (-1000.0) 1000.0)
    let dir       = L.normalize (V3 x y 1.0)
        ray       = Ray_ (V3_ 0.0 0.0 0.0) (constant dir)
        plane     = makePlane (V3 0.0 0.0 1.0) (V3 0.0 0.0 (-1.0))

        -- Now calculate the distance to the plane based on the angle
        cos_angle = L.dot dir (V3 0.0 0.0 1.0)
        dist      = 1.0 / cos_angle

    -- Check if there was a hit
    evalExp (distanceTo ray plane)
      === if dist >= 0.0 then Just dist else Nothing
  , testProperty "Backface culling Angles" $ property $ do
    x <- forAll $ Gen.float (Range.linearFrac (-1000.0) 1000.0)
    y <- forAll $ Gen.float (Range.linearFrac (-1000.0) 1000.0)
    let dir   = constant $ L.normalize (V3 x y 1.0)
        ray   = Ray_ (V3_ 0.0 0.0 0.0) dir
        plane = makePlane (V3 0.0 0.0 1.0) (V3 0.0 0.0 1.0)

    evalExp (distanceTo ray plane) === Nothing
  ]

-- | Evaluate a single Accelerate expression to a value. This is needed because
-- we can't compare unevaluated expressions directly.
--
-- TODO: Refactor the tests to evaluate both the CUDA and CPU backends
evalExp :: Elt a => Exp a -> a
evalExp e = head . A.toList $ run (unit e)

makeSphere :: V3 Float -> Float -> Exp Sphere
makeSphere pos diameter = constant $ Sphere { _spherePosition = pos
                                            , _sphereRadius   = diameter
                                            , _sphereMaterial = dummyMaterial
                                            }

-- | Generate a dummy plane by its pos and normal.
makePlane :: V3 Float -> V3 Float -> Exp Plane
makePlane pos nor = constant $ Plane { _planeDirection = nor
                                     , _planePosition  = pos
                                     , _planeMaterial  = dummyMaterial
                                     }

-- | A placeholder material for use in tests.
dummyMaterial :: Material
dummyMaterial = Material { _materialColor       = V3 1.0 1.0 1.0
                         , _materialIlluminance = 1.0
                         , _materialBrdf        = Matte 1.0
                         }

-- | Round a number to a certain number of places. Uesful in comparisons.
roundTo :: Int -> Float -> Float
roundTo places i = fromInteger (round $ i * (10 ^ places)) / (10.0 ^^ places)

-- * Generators

point :: Gen (V3 Float)
point = v3 (Range.linearFracFrom 0 (-1000) 1000)

v3 :: Range Float -> Gen (V3 Float)
v3 r = V3 <$> Gen.float r <*> Gen.float r <*> Gen.float r
