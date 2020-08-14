module Scene.World where

import qualified Data.Array.Accelerate         as A
import           Linear

import           Scene.Objects

initialCamera :: Camera
initialCamera = Camera { _cameraPosition = V3 1.0 (-1.6) (-4.8)
                       , _cameraRotation = V3 0.314 (-0.314) 0.0
                       , _cameraFov      = 90
                       }

-- | These are the objects and lights used in our static scene.
mainScene :: Scene
mainScene = Scene (map A.constant spheres') (map A.constant planes')
 where
  spheres' =
    [ Sphere
      { _spherePosition = V3 2.0 2.0 (-14.0)
      , _sphereRadius   = 5.0
      , _sphereMaterial = Material { _materialColor       = V3 1.0 0.3 0.3
                                   , _materialIlluminance = 0.0
                                   , _materialBrdf        = Matte 0.8
                                   }
      }
    , Sphere
      { _spherePosition = V3 6.0 2.0 (-9.0)
      , _sphereRadius   = 1.5
      , _sphereMaterial = Material { _materialColor       = V3 0.0 0.4 0.0
                                   , _materialIlluminance = 0.0
                                   , _materialBrdf        = Matte 0.9
                                   }
      }
    , Sphere
      { _spherePosition = V3 4.5 1.0 (-9.0)
      , _sphereRadius   = 0.5
      , _sphereMaterial = Material { _materialColor       = V3 0.4 0.4 1.0
                                   , _materialIlluminance = 0.0
                                   , _materialBrdf        = Glossy 1.0
                                   }
      }
    , Sphere
      { _spherePosition = V3 16.0 (-2.05) (-20.0)
      , _sphereRadius   = 0.9
      , _sphereMaterial = Material { _materialColor       = V3 0.8 0.8 0.8
                                   , _materialIlluminance = 6942.0
                                   , _materialBrdf        = Glossy 0.5
                                   }
      }
    , Sphere
      { _spherePosition = V3 5.0 10.0 4.0
      , _sphereRadius   = 2.0
      , _sphereMaterial = Material { _materialColor       = V3 0.99 0.84 0.12
                                   , _materialIlluminance = 4420.0
                                   , _materialBrdf        = Matte 1.0
                                   }
      }
    ]
  planes' =
    [ Plane
      { _planeDirection = V3 0.0 1.0 0.0
      , _planePosition  = V3 0.0 (-3.0) 0.0
      , _planeMaterial  = Material { _materialColor       = V3 0.43 0.95 0.5
                                   , _materialIlluminance = 0.0
                                   , _materialBrdf        = Matte 1.5
                                   }
      }
    , Plane
      { _planeDirection = V3 0.0 (-1.0) 0.0
      , _planePosition  = V3 0.0 15.0 0.0
      , _planeMaterial  = Material { _materialColor       = V3 0.26 0.68 0.88
                                   , _materialIlluminance = 0.0
                                   , _materialBrdf        = Glossy 0.9
                                   }
      }
    ]
