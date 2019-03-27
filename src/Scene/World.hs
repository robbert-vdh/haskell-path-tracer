module Scene.World where

import Scene.Objects

import Linear

getStartCamera :: Camera
getStartCamera =
  Camera
    { _cameraPosition = V3 1.0 (-1.6) (-4.8)
   -- TODO: Use proper angles here. This should be implemented using quaternions
   --       and euler angles, but since we don't have any way to rotate the
   --       camera yet we'll just use looking direciton instead.
   -- , _cameraDirection = normalize $ V3 0.3 (-0.3) 0.0 -- These are Euler angles
    , _cameraDirection = normalize $ V3 0.1 (-0.01) (-1.0)
    , _cameraFov = 45
    }

-- | These are the objects and lights used in our static scene.
getObjects :: Scene
getObjects =
  Scene
    [ Sphere
        { _spherePosition = V3 2.0 2.0 (-14.0)
        , _sphereRadius = 5.0
        , _sphereMaterial =
            Material
              { _materialColor = V3 0.0 1.0 0.0
              , _materialSpecularity = 0.7
              , _materialIlluminance = 0.0
              }
        }
    , Sphere
        { _spherePosition = V3 6.0 2.0 (-9.0)
        , _sphereRadius = 1.5
        , _sphereMaterial =
            Material
              { _materialColor = V3 0.0 1.0 0.0
              , _materialSpecularity = 0.4
              , _materialIlluminance = 1.0
              }
        }
    , Sphere
        { _spherePosition = V3 4.5 1.0 (-9.5)
        , _sphereRadius = 0.3
        , _sphereMaterial =
            Material
              { _materialColor = V3 0.0 0.0 1.0
              , _materialSpecularity = 0.0
              , _materialIlluminance = 1.5
              }
        }
    , Sphere
        { _spherePosition = V3 12.0 (-2.4) (-20.0)
        , _sphereRadius = 0.5
        , _sphereMaterial =
            Material
              { _materialColor = V3 0.5 0.5 0.5
              , _materialSpecularity = 0.0
              , _materialIlluminance = 0.0
              }
        }
    ]
    [ Plane
        { _planeDirection = V3 0.0 1.0 0.0
        , _planePosition = V3 0.0 (-3.0) 0.0
        , _planeMaterial =
            Material
              { _materialColor = V3 0.0 0.0 0.0
              , _materialSpecularity = 0.7
              , _materialIlluminance = 0.0
              }
        }
    , Plane
        { _planeDirection = V3 0.0 (-1.0) 0.0
        , _planePosition = V3 0.0 10.0 0.0
        , _planeMaterial =
            Material
              { _materialColor = V3 0.0 0.0 0.0
              , _materialSpecularity = 0.1
              , _materialIlluminance = 10.0
              }
        }
    ]

getBasicObjects :: Scene
getBasicObjects = Scene [sp] [pl]
  where
    sp =
      Sphere
        { _spherePosition = V3 0.0 0.0 (-2.0)
        , _sphereRadius = 1.0
        , _sphereMaterial =
            Material
              { _materialColor = V3 1.0 1.0 1.0
              , _materialSpecularity = 0.7
              , _materialIlluminance = 0.5
              }
        }
    pl =
      Plane
        { _planeDirection = V3 0.0 (-1.0) 0.0
        , _planePosition = V3 0.0 (-1.0) 0.0
        , _planeMaterial =
            Material
              { _materialColor = V3 1.0 1.0 1.0
              , _materialSpecularity = 0.1
              , _materialIlluminance = 0.8
              }
        }
