{-# LANGUAGE TypeOperators #-}

-- | Small utility functions for conversions.

module Util where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Data.Functor as A
import Data.Array.Accelerate.Linear as A

import qualified Prelude as P

import Scene.Objects
import Scene.World (getStartCamera)

-- * Functions

-- | Convert an integer vector to a float vector. This is only used when
-- converting between rasterization and world spaces.
vecToFloat ::
     (Functor f, Elt (f Int), Elt (f Float)) => Exp (f Int) -> Exp (f Float)
vecToFloat = fmap toFloating

-- * Definitions

-- | The dimensions of the screen. These are hard coded for efficiency's sake
-- even though the window could be resizable with minor adjustments.
screenWidth, screenHeight :: Int32
screenWidth = 800
screenHeight = 600

-- | The aspect ratio of the output. Used in the FOV and perspective
-- calculations.
screenAspect :: Exp Float
screenAspect = P.fromIntegral screenWidth / P.fromIntegral screenHeight

-- | The dimensions of the screen as a float vector. Note that the screen height
-- has been inverted here as the @y@ axis changes orientation when converting
-- between rasterization and screen spaces.
screenSize :: Exp (V2 Float)
screenSize =
  V2' (P.fromIntegral screenWidth) (P.fromIntegral $ negate screenHeight)

-- | The output matrix initialized with all zero values. This is used during the
-- initialization and after moving the camera.
initialOutput :: A.Matrix Color
initialOutput = fromFunction screenShape $ const $ V3 0.0 0.0 0.0

-- | A matrix containing coordinates for every pixel on the screen. This is used
-- to cast the actual rays.
--
-- TODO: Add RNG seeds here
screenPixels :: (A.Matrix (V2 Int, Int))
screenPixels = A.fromFunction screenShape $ \(Z :. y :. x) -> (V2 x y, 1)

-- | The size of the output as an array shape.
screenShape :: Z :. Int :. Int
screenShape = Z :. P.fromIntegral screenHeight :. P.fromIntegral screenWidth

-- TODO: Replace this with some actual value
theCamera :: A.Exp Camera
theCamera = A.constant getStartCamera