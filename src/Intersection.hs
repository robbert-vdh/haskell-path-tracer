{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}

-- | This module defines a common interface for doing calculations on
-- primitives.
module Intersection where

import Data.Array.Accelerate
import Data.Array.Accelerate.Control.Lens
import Data.Array.Accelerate.Data.Maybe
import Data.Array.Accelerate.Linear

import Scene.Objects

class Primitive p
  -- | Distance to the primitive if it intersects. Returns a 'Just t' if ray
  -- intersects with the primitive at @rayOrigin + rayDIrection * t@.
  where
  distanceTo :: Exp RayF -> Exp p -> Exp (Maybe Float)
  -- | Get intersection point, normal and material for a sphere hit. Assumes
  -- there is a hit.
  hit :: Exp RayF -> Exp Float -> Exp p -> Exp (RayF, Material)
  -- | Calculate the normal of a primitive at the specified position in world
  -- space. The normal should be pointing outwards.
  normal :: Exp Position -> Exp p -> Exp Direction

  default hit :: HasMaterial p Material =>
    Exp RayF -> Exp Float -> Exp p -> Exp (RayF, Material)
  hit ~(Ray' o d) t p =
    T2 (Ray' hitPosition (normal hitPosition p)) (p ^. material)
    where
      hitPosition = o + (d ^* t)

instance Primitive Sphere where
  distanceTo ~(Ray' ori dir) ~(Sphere' pos rad _) =
    if d_cp >= rad || sep `dot` dir <= 0 -- miss
      then nothing
      else just dist
    where
      p = ori + ((pos - ori) `dot` dir) *^ dir
      d_cp = norm (p - pos)
      sep = p - ori
      dist = norm sep - sqrt (rad ** 2 - d_cp ** 2)

  normal pos ~(Sphere' ori _ _) = normalize (pos - ori)

instance Primitive Plane where
  distanceTo ~(Ray' ori dir) ~(Plane' pos nor _) =
    if x >= 0
      then nothing
      else just dist
    where
      x = dir `dot` pos
      dist = ((pos - ori) `dot` nor) / x

  normal _ ~(Plane' _ nor _) = nor
