module Scene.World where

import Scene.Objects

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Linear.V3

getObjects :: (Array DIM1 Sphere, Array DIM1 Plane)
getObjects = (spheres, planes)
    where 
        spheres = A.fromList (Z :. 1) [
            Sphere (V3 0.0 (-150.0) (-100)) -- pos
                   0.8                      -- radius
                   (V3 1.0 1.0 1.0)         -- color
                   0.75                     -- specularity
            ]
        planes  = A.fromList (Z :. 1) [
            Plane (V3 0.0 100.0 0.0) -- pos
                  (V3 0.0 0.0 0.0)   -- direction
                  (V3 1.0 1.0 1.0)   -- color
                  0.15               -- specularity
            ]