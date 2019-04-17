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

import Lib (run)
import Scene.Intersection
import Scene.Objects

tests :: TestTree
tests = testGroup "Scene.Intersection" [sphereTests]

sphereTests :: TestTree
sphereTests =
  testGroup
    "Sphere"
    [ testProperty "intersection (makeSphere (x, 0, x), x) == (0, 0, x)" $
      property $ do
        diameter <- forAll $ Gen.float (Range.linearFrac 0.0 100.0)
        let sphere = makeSphere (V3 diameter 0 diameter) diameter
            ray = Ray' (V3' 0.0 0.0 0.0) (V3' 0.0 0.0 1.0)
            distance = fromJust $ distanceTo ray sphere
            A.T2 (Ray' hitPos _) _ = hit ray distance sphere

        evalExp hitPos === V3 0.0 0.0 diameter
    ]

-- |
evalExp :: Elt a => Exp a -> a
evalExp e = head $ A.toList $ run (unit e)

makeSphere :: V3 Float -> Float -> Exp Sphere
makeSphere pos diameter =
  constant $
  Sphere
    { _spherePosition = pos
    , _sphereRadius = diameter
    , _sphereMaterial =
        Material
          { _materialColor = V3 1.0 1.0 1.0
          , _materialIlluminance = 1.0
          , _materialBrdf = Diffuse 1.0
          }
    }
