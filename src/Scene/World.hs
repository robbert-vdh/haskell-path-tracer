module Scene.World where

import Scene.Objects

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Linear.V3

-- TODO: Use records for clarity and port the scene from the other path tracer
--       over
getObjects :: Scene
getObjects =
  Scene
    (A.fromList
       (Z :. 1)
       [Sphere (V3 0.0 (-150.0) (-100)) 0.8 (V3 1.0 1.0 1.0) 0.75])
    (A.fromList
       (Z :. 1)
       [Plane (V3 0.0 100.0 0.0) (V3 0.0 0.0 0.0) (V3 1.0 1.0 1.0) 0.15])
    (A.fromList (Z :. 1) [Light (V3 0.0 100.0 0.0) (V3 1.0 1.0 1.0)])
