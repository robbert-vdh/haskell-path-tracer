module Scene.World where

import Scene.Objects

import Data.Array.Accelerate as A
import Linear

getStartCamera :: Camera
getStartCamera = Camera
   -- { _cameraPosition = V3 1.0 (-1.6) (-4.8)
   { _cameraPosition = V3 0 0 0
   -- TODO: Use proper angles here. This should be implemented using quaternions
   --       and euler angles, but since we don't have any way to rotate the
   --       camera yet we'll just use looking direciton instead.
   -- , _cameraDirection = normalize $ V3 0.3 (-0.3) 0.0
   -- , _cameraDirection = normalize $ V3 (-0.1) (-0.1) (-1.0)
   , _cameraDirection = normalize $ V3 0 0 (-1)
   , _cameraFov = 90
   }

-- | These are the objects and lights used in our static scene.
getObjects :: Scene
getObjects =
  Scene
    (A.fromList
       (Z :. 4)
       [ Sphere
           { _spherePosition = V3 2.0 2.0 (-14.0)
           , _sphereRadius = 5.0
           , _sphereColor = V3 0.0 1.0 0.0
           , _sphereSpecularity = 0.7
           }
       , Sphere
           { _spherePosition = V3 6.0 2.0 (-9.0)
           , _sphereRadius = 1.5
           , _sphereColor = V3 0.0 1.0 0.0
           , _sphereSpecularity = 0.4
           }
       , Sphere
           { _spherePosition = V3 4.5 1.0 (-9.5)
           , _sphereRadius = 0.3
           , _sphereColor = V3 0.0 0.0 1.0
           , _sphereSpecularity = 0.0
           }
       , Sphere
           { _spherePosition = V3 12.0 (-2.4) (-20.0)
           , _sphereColor = V3 0.5 0.5 0.5
           , _sphereRadius = 0.5
           , _sphereSpecularity = 0.0
           }
       ])
    (A.fromList
       (Z :. 2)
       [ Plane
           { _planeDirection = V3 0.0 1.0 0.0
           , _planePosition = V3 0.0 (-3.0) 0.0
           , _planeColor = V3 0.0 0.0 0.0
           , _planeSpecularity = 0.7
           }
       , Plane
           { _planeDirection = V3 0.0 (-1.0) 0.0
           , _planePosition = V3 0.0 10.0 0.0
           , _planeColor = V3 0.0 0.0 0.0
           , _planeSpecularity = 0.1
           }
       ])
    (A.fromList
       (Z :. 1)
       [ Light
           {_lightPosition = V3 0.0 6.0 (-1.0), _lightColor = V3 1.0 1.0 1.0}
       ])
