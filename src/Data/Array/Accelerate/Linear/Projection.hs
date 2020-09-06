{-# LANGUAGE FlexibleContexts #-}

-- | Functions from 'Linear.Projection' lifted to Accelerate terms.
--
-- These function definitions are copied directly from their corresponding
-- functions in 'Linear.Projection' but with their terms lifted to 'Exp's. This
-- is needed as the 'Epsilon' type classes from 'Linear.Epsilon' and
-- 'Data.Array.Accelerate.Linear.Epsilon' are not compatible, and as such we
-- can't make use of the @lift*@ functions.
module Data.Array.Accelerate.Linear.Projection where

import           Data.Array.Accelerate
import           Data.Array.Accelerate.Control.Lens
import           Data.Array.Accelerate.Linear

import qualified Prelude                       as P
                                                ( )

{-# ANN inverseInfinitePerspective "HLint: ignore Reduce duplication" #-}

-- | Build a look at view matrix
lookAt
  :: (Epsilon a, Floating a)
  => Exp (V3 a) -- ^ Eye
  -> Exp (V3 a) -- ^ Center
  -> Exp (V3 a) -- ^ Up
  -> Exp (M44 a)
lookAt eye center up = V4_ (V4_ (xa ^. _x) (xa ^. _y) (xa ^. _z) xd)
                           (V4_ (ya ^. _x) (ya ^. _y) (ya ^. _z) yd)
                           (V4_ (-za ^. _x) (-za ^. _y) (-za ^. _z) zd)
                           (V4_ 0 0 0 1)
 where
  za = normalize $ center - eye
  xa = normalize $ cross za up
  ya = cross xa za
  xd = -dot xa eye
  yd = -dot ya eye
  zd = dot za eye

-- | Lookat function as defined by scratchpixel added for debugging
--
-- https://www.scratchapixel.com/lessons/mathematics-physics-for-computer-graphics/lookat-function
lookAtScratch
  :: (Epsilon a, Floating a)
  => Exp (V3 a) -- ^ From
  -> Exp (V3 a) -- ^ To
  -> Exp (V3 a) -- ^ Tmp
  -> Exp (M44 a)
lookAtScratch from' to' tmp = V4_
  (V4_ (right ^. _x) (right ^. _y) (right ^. _z) 0)
  (V4_ (up ^. _x) (up ^. _y) (up ^. _z) 0)
  (V4_ (forward ^. _x) (forward ^. _y) (forward ^. _z) 0)
  (V4_ (from' ^. _x) (from' ^. _y) (from' ^. _z) 0)
 where
  forward = normalize (from' - to')
  right   = normalize tmp `cross` forward
  up      = forward `cross` right

-- | Build a matrix for a symmetric perspective-view frustum
perspective
  :: Floating a
  => Exp a -- ^ FOV (y direction, in radians)
  -> Exp a -- ^ Aspect ratio
  -> Exp a -- ^ Near plane
  -> Exp a -- ^ Far plane
  -> Exp (M44 a)
perspective fovy aspect near far = V4_ (V4_ x 0 0 0)
                                       (V4_ 0 y 0 0)
                                       (V4_ 0 0 z w)
                                       (V4_ 0 0 (-1) 0)
 where
  tanHalfFovy = tan $ fovy / 2
  x           = 1 / (aspect * tanHalfFovy)
  y           = 1 / tanHalfFovy
  fpn         = far + near
  fmn         = far - near
  oon         = 0.5 / near
  oof         = 0.5 / far
  -- z = 1 / (near/fpn - far/fpn) -- would be better by .5 bits
  z           = -fpn / fmn
  w           = 1 / (oof - oon) -- 13 bits error reduced to 0.17
  -- w = -(2 * far * near) / fmn

-- | Build a matrix for a symmetric perspective-view frustum with a far plane at
-- infinite
infinitePerspective
  :: Floating a
  => Exp a -- ^ FOV (y direction, in radians)
  -> Exp a -- ^ Aspect Ratio
  -> Exp a -- ^ Near plane
  -> Exp (M44 a)
infinitePerspective fovy a n = V4_ (V4_ x 0 0 0)
                                   (V4_ 0 y 0 0)
                                   (V4_ 0 0 (-1) w)
                                   (V4_ 0 0 (-1) 0)
 where
  t = n * tan (fovy / 2)
  b = -t
  l = b * a
  r = t * a
  x = (2 * n) / (r - l)
  y = (2 * n) / (t - b)
  w = -2 * n

inverseInfinitePerspective
  :: Floating a
  => Exp a -- ^ FOV (y direction, in radians)
  -> Exp a -- ^ Aspect Ratio
  -> Exp a -- ^ Near plane
  -> Exp (M44 a)
inverseInfinitePerspective fovy a n = V4_ (V4_ rx 0 0 0)
                                          (V4_ 0 ry 0 0)
                                          (V4_ 0 0 0 (-1))
                                          (V4_ 0 0 rw (-rw))
 where
  t   = n * tan (fovy / 2)
  b   = -t
  l   = b * a
  r   = t * a
  hrn = 0.5 / n
  rx  = (r - l) * hrn
  ry  = (t - b) * hrn
  rw  = -hrn
