{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RebindableSyntax #-}

-- | This module defines a common interface for doing calculations on
-- primitives.
module Intersection where

import Data.Array.Accelerate
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Linear

import Scene.Objects

class Primitive p where
  -- | Distance to the primitive if it intersects. Returns a 'Just t' if ray
  -- intersects with the primitive at @rayOrigin + rayDIrection * t@.
  distanceTo :: Exp p -> Exp RayF -> Exp (Maybe Float)
  -- | Get intersection point, normal and material for a sphere hit. Assumes
  -- there is a hit.
  hit :: Exp p -> Exp RayF -> Exp Float -> Exp (RayF, Material)

instance Primitive Sphere where
  distanceTo ~(Sphere' pos rad _) ~(Ray' ori dir) =
    if miss
      then nothing
      else just dist
    where
      p = ori + ((pos - ori) `dot` dir) *^ dir
      d_cp = norm (p - pos)
      sep = p - ori
      miss = d_cp >= rad || sep `dot` dir <= 0
      dist = norm sep - sqrt (rad ** 2 - d_cp ** 2)

  hit = undefined

instance Primitive Plane where
  distanceTo ~(Plane' pos nor _) ~(Ray' or dir) =
    if x >= 0
      then nothing
      else just dist
    where
      x = dir `dot` pos
      dist = ((pos - or) `dot` nor) / x

  hit = undefined
