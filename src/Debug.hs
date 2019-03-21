module Debug where

import Data.Array.Accelerate hiding ((++))
import Data.Array.Accelerate.Linear
import qualified Data.Array.Accelerate.Interpreter as Interpreter
import Data.List (intercalate)

import Prelude as P

import Lib (runN)
import Scene (render)
import Util

-- | Compute a single pixel using the interpreter. This allows you to use
-- 'Debug.Trace' to debug expressions.
computeDebug :: (Int, Int) -> IO ()
computeDebug (x, y) = do
  let pixels = fromList (Z :. 1 :. 1) [(V2 x y, 1)]
      result = toList $ Interpreter.runN (render theCamera) pixels initialOutput

  putStrLn $ show (x, y) ++ " -> " ++ show result

-- | Compute and print the colour values for the given screen pixels.
computePixels :: [(Int, Int)] -> IO ()
computePixels points = do
  let pixels =
        fromList (Z :. 1 :. P.length points) $
        P.map (\(x, y) -> (V2 x y, 1)) points
      result = toList $ runN (render theCamera) pixels initialOutput

  putStrLn $
    intercalate "\n" $
    P.zipWith (\p r -> show p ++ " -> " ++ show r) points result
