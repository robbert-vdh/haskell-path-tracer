{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scene.Objects where

import Data.Typeable
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Linear as A
import qualified Prelude

-- * Objects

type Position = V3 Float

type Direction = V3 Float

type Colour = V3 Float

type Noraml = (Position, Direction)

data Sphere =
  Sphere Position
         Float
         Colour
         Float
  deriving (Prelude.Eq, Show, Typeable)

data Plane =
  Plane Position
        Direction
        Colour
        Float
  deriving (Prelude.Eq, Show, Typeable)

-- * Instances
-- ** Sphere

instance Elt Sphere where
  type EltRepr Sphere = EltRepr (Position, Float, Colour, Float)
  eltType = eltType @(Position, Float, Colour, Float)
  toElt t = let (p, r, c, s) = toElt t in Sphere p r c s
  fromElt (Sphere p r c s) = fromElt (p, r, c, s)

instance (cst Float, cst (V3 Float)) => IsProduct cst Sphere where
  type ProdRepr Sphere = ProdRepr (Position, Float, Colour, Float)
  toProd t = let (p, r, c, s) = toProd @cst t in Sphere p r c s
  fromProd (Sphere p r c s) = fromProd @cst (p, r, c, s)
  prod = prod @cst @(Position, Float, Colour, Float)

instance Lift Exp Sphere where
  type Plain Sphere = Sphere
  lift = constant

-- ** Sphere

instance Elt Plane where
  type EltRepr Plane = EltRepr (Position, Direction, Colour, Float)
  eltType = eltType @(Position, Direction, Colour, Float)
  toElt t = let (p, r, c, s) = toElt t in Plane p r c s
  fromElt (Plane p r c s) = fromElt (p, r, c, s)

instance (cst Float, cst (V3 Float)) => IsProduct cst Plane where
  type ProdRepr Plane = ProdRepr (Position, Direction, Colour, Float)
  toProd t = let (p, r, c, s) = toProd @cst t in Plane p r c s
  fromProd (Plane p r c s) = fromProd @cst (p, r, c, s)
  prod = prod @cst @(Position, Direction, Colour, Float)

instance Lift Exp Plane where
  type Plain Plane = Plane
  lift = constant
