import           Test.Tasty

import qualified Scene.Intersection.Tests

main :: IO ()
main = defaultMain $ testGroup "tests" [Scene.Intersection.Tests.tests]
