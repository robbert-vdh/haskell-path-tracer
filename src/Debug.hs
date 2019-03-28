module Debug where

import Data.Array.Accelerate hiding ((++), uncurry)
import qualified Data.Array.Accelerate.Interpreter as Interpreter
import Data.Array.Accelerate.Linear
import Data.List (intercalate)

import Prelude as P

import Lib (runN)
import Scene (render)
import Scene.World (initialCamera)
import Util

-- | Compute a single pixel using the interpreter. This allows you to use
-- 'Debug.Trace' to debug expressions.
computeDebug :: (Int, Int) -> IO ()
computeDebug (x, y) = do
  seeds <- initialOutput
  let pixels = fromList (Z :. 1 :. 1) [V2 x y]
      result = toList $ Interpreter.runN (render $ constant initialCamera) pixels seeds

  putStrLn $ show (x, y) ++ " -> " ++ show result

-- | Compute and print the colour values for the given screen pixels.
computePixels :: [(Int, Int)] -> IO ()
computePixels points = do
  seeds <- initialOutput
  let pixels = fromList (Z :. 1 :. P.length points) $ P.map (uncurry V2) points
      result = toList $ runN (render $ constant initialCamera) pixels seeds

  putStrLn $
    intercalate "\n" $
    P.zipWith (\p r -> show p ++ " -> " ++ show r) points result
