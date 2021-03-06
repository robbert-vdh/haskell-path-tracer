{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RebindableSyntax #-}

-- | This module defines a common interface for doing calculations on
-- primitives.
module Scene.Intersection where

import           Data.Array.Accelerate
import           Data.Array.Accelerate.Control.Lens
import           Data.Array.Accelerate.Linear

import           Scene.Objects

class Primitive p where
  -- | Distance to the primitive if it intersects. Returns a 'Just t' if ray
  -- intersects with the primitive at @rayOrigin + rayDIrection * t@.
  distanceTo :: Exp RayF -> Exp p -> Exp (Maybe Float)
  -- | Calculate the normal of a primitive at the specified position in world
  -- space. The normal should be pointing outwards.
  normal :: Exp Point -> Exp p -> Exp Direction
  -- | Get intersection point, normal and material for a sphere hit. Assumes
  -- there is a hit.
  hit :: Exp RayF -> Exp Float -> Exp p -> Exp (NormalP, Material)

  default hit :: HasMaterial p Material =>
    Exp RayF -> Exp Float -> Exp p -> Exp (NormalP, Material)
  hit (Ray_ o d) t p =
    T2 (Ray_ hitPosition (normal hitPosition p)) (p ^. material)
    where
      hitPosition = o + (d ^* t)

instance Primitive Sphere where
  -- | Calculate distance to a sphere, implemented in the same manner as
  -- in the scratchpixel tutorial
  --
  -- https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-sphere-intersection
  distanceTo (Ray_ ori dir) ~(Sphere_ pos rad _) =
    if tca < 0 || d2 > (rad ** 2) || t < 0 then Nothing_ else Just_ t
   where
    l   = pos - ori
    tca = l `dot` dir
    d2  = (l `dot` l) - (tca * tca)
    thc = sqrt (rad ** 2 - d2)
    t0  = tca - thc
    t1  = tca + thc
    t   = min t0 t1

  normal pos (Sphere_ ori _ _) = normalize (pos - ori)

instance Primitive Plane where
  -- | Calculate distance to plane, implemented in the same manner as in the
  -- scratchpixel tutorial.
  --
  -- https://www.scratchapixel.com/lessons/3d-basic-rendering/minimal-ray-tracer-rendering-simple-shapes/ray-plane-and-ray-disk-intersection
  distanceTo (Ray_ ori dir) ~(Plane_ pos nor _) = if denom > 1e-6 || dist < 0
    then Nothing_
    else Just_ dist
   where
    denom = dir `dot` nor
    dist  = ((pos - ori) `dot` nor) / denom

  normal _ (Plane_ _ nor _) = nor
