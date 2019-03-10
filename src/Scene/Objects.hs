{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scene.Objects where

import Data.Array.Accelerate as A
import Data.Array.Accelerate.Array.Sugar
import Data.Array.Accelerate.Control.Lens
import Data.Array.Accelerate.Linear as A
import Data.Array.Accelerate.Product
import Data.Array.Accelerate.Smart
import Data.Typeable
import qualified Prelude

-- * Objects

type Position = V3 Float

type Direction = V3 Float

type Colour = V3 Float

type Noraml = (Position, Direction)

data Sphere = Sphere
  { _spherePosition :: Position
  , _sphereRadius :: Float
  , _sphereColour :: Colour
  , _sphereSpecularity :: Float
  } deriving (Prelude.Eq, Show, Typeable)

data Plane = Plane
  { _planePosition :: Position
  , _planeDirection :: Direction
  , _planeColour :: Colour
  , _planeSpecularity :: Float
  } deriving (Prelude.Eq, Show, Typeable)

-- * Lenses
--
-- Since Sphere and Plane do not have a type parameter we can't make use
-- 'liftLens' or `unlift`, so we'll just define some simple getters ourselves.

class HasPosition t where
  position :: Getter (Exp t) (Exp Position)
instance HasPosition Sphere where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t
instance HasPosition Plane where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t

class HasColour t where
  colour :: Getter (Exp t) (Exp Colour)
instance HasColour Sphere where
  colour = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t
instance HasColour Plane where
  colour = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t

class HasSpecularity t where
  specularity :: Getter (Exp t) (Exp Float)
instance HasSpecularity Sphere where
  specularity = to $ \t -> Exp $ ZeroTupIdx `Prj` t
instance HasSpecularity Plane where
  specularity = to $ \t -> Exp $ ZeroTupIdx `Prj` t

radius :: Getter (Exp Sphere) (Exp Float)
radius = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t

direction :: Getter (Exp Plane) (Exp Direction)
direction = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t

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
