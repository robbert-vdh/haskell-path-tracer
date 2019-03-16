{-# LANGUAGE FlexibleContexts #-}

-- | Functions from 'Linear.Projection' lifted to Accelerate terms.
--
-- These function definitions are copied directly from their corresponding
-- functions in 'Linear.Projection' but with their terms lifted to 'Exp's. This
-- is needed as the 'Epsilon' type classes from 'Linear.Epsilon' and
-- 'Data.Array.Accelerate.Linear.Epsilon' are not compatible, and as such we
-- can't make use of the `liftN` functions.

module Data.Array.Accelerate.Linear.Projection where

import Data.Array.Accelerate
import Data.Array.Accelerate.Control.Lens
import Data.Array.Accelerate.Linear

import qualified Prelude as P ()

-- | Build a look at view matrix
lookAt ::
     (Epsilon a, Floating a)
  => Exp (V3 a) -- ^ Eye
  -> Exp (V3 a) -- ^ Center
  -> Exp (V3 a) -- ^ Up
  -> Exp (M44 a)
lookAt eye center up =
  V4'
    (V4' (xa ^. _x) (xa ^. _y) (xa ^. _z) xd)
    (V4' (ya ^. _x) (ya ^. _y) (ya ^. _z) yd)
    (V4' (-za ^. _x) (-za ^. _y) (-za ^. _z) zd)
    (V4' 0 0 0 1)
  where
    za = normalize $ center - eye
    xa = normalize $ cross za up
    ya = cross xa za
    xd = -dot xa eye
    yd = -dot ya eye
    zd = dot za eye

-- | Build a matrix for a symmetric perspective-view frustum with a far plane at
-- infinite
infinitePerspective ::
     Floating a
  => Exp a -- ^ FOV (y direction, in radians)
  -> Exp a -- ^ Aspect Ratio
  -> Exp a -- ^ Near plane
  -> Exp (M44 a)
infinitePerspective fovy a n =
  V4' (V4' x 0 0 0)
      (V4' 0 y 0 0)
      (V4' 0 0 (-1) w)
      (V4' 0 0 (-1) 0)
  where
    t = n * tan (fovy / 2)
    b = -t
    l = b * a
    r = t * a
    x = (2 * n) / (r - l)
    y = (2 * n) / (t - b)
    w = -2 * n
