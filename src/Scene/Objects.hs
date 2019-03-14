{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
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
type Color = V3 Float
type Noraml = (Position, Direction)

data Sphere = Sphere
  { _spherePosition :: Position
  , _sphereRadius :: Float
  , _sphereColor :: Color
  , _sphereSpecularity :: Float
  } deriving (Prelude.Eq, Show, Typeable)

data Plane = Plane
  { _planePosition :: Position
  , _planeDirection :: Direction
  , _planeColor :: Color
  , _planeSpecularity :: Float
  } deriving (Prelude.Eq, Show, Typeable)

data Light = Light
  { _lightPosition :: Position
  , _lightColor :: Color
  } deriving (Prelude.Eq, Show, Typeable)

data Scene = Scene
  { _sceneSpheres :: A.Vector Sphere
  , _scenePlanes :: A.Vector Plane
  , _sceneLights :: A.Vector Light
  } deriving Typeable

data Ray = Ray
  { _rayOrigin :: Position
  , _rayDirection :: Direction
  } deriving (Prelude.Eq, Show, Typeable)

data Camera = Camera
  { _cameraPosition :: Position
  , _cameraDirection :: Direction
  , _cameraFov :: Int
  } deriving (Prelude.Eq, Show, Typeable)

-- * Lenses
--
-- Since Sphere, Plane and Light do not have a type parameter we can't make use
-- 'liftLens' or `unlift`, so we'll just define some simple getters ourselves.

makeFields ''Scene

class HasPosition t where
  position :: Getter (Exp t) (Exp Position)
instance HasPosition Sphere where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t
instance HasPosition Plane where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t
instance HasPosition Light where
  position = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t
instance HasPosition Camera where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t

class HasColor t where
  color :: Getter (Exp t) (Exp Color)
instance HasColor Sphere where
  color = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t
instance HasColor Plane where
  color = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t
instance HasColor Light where
  color = to $ \t -> Exp $ ZeroTupIdx `Prj` t

class HasSpecularity t where
  specularity :: Getter (Exp t) (Exp Float)
instance HasSpecularity Sphere where
  specularity = to $ \t -> Exp $ ZeroTupIdx `Prj` t
instance HasSpecularity Plane where
  specularity = to $ \t -> Exp $ ZeroTupIdx `Prj` t

class HasDirection t where
  direction :: Getter (Exp t) (Exp Direction)
instance HasDirection Plane where
  direction = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
instance HasDirection Ray where
  direction = to $ \t -> Exp $ ZeroTupIdx `Prj` t
instance HasDirection Camera where
  direction = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t

radius :: Getter (Exp Sphere) (Exp Float)
radius = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t

origin :: Getter (Exp Ray) (Exp Position)
origin = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t

fov :: Getter (Exp Camera) (Exp Int)
fov = to $ \t -> Exp $ ZeroTupIdx `Prj` t

-- * Instances
-- ** Sphere
instance Elt Sphere where
  type EltRepr Sphere = EltRepr (Position, Float, Color, Float)
  eltType = eltType @(Position, Float, Color, Float)
  toElt t = let (p, r, c, s) = toElt t in Sphere p r c s
  fromElt (Sphere p r c s) = fromElt (p, r, c, s)

instance (cst Float, cst (V3 Float)) => IsProduct cst Sphere where
  type ProdRepr Sphere = ProdRepr (Position, Float, Color, Float)
  toProd t = let (p, r, c, s) = toProd @cst t in Sphere p r c s
  fromProd (Sphere p r c s) = fromProd @cst (p, r, c, s)
  prod = prod @cst @(Position, Float, Color, Float)

instance Lift Exp Sphere where
  type Plain Sphere = Sphere
  lift = constant

-- ** Plane
instance Elt Plane where
  type EltRepr Plane = EltRepr (Position, Direction, Color, Float)
  eltType = eltType @(Position, Direction, Color, Float)
  toElt t = let (p, r, c, s) = toElt t in Plane p r c s
  fromElt (Plane p r c s) = fromElt (p, r, c, s)

instance (cst Float, cst (V3 Float)) => IsProduct cst Plane where
  type ProdRepr Plane = ProdRepr (Position, Direction, Color, Float)
  toProd t = let (p, r, c, s) = toProd @cst t in Plane p r c s
  fromProd (Plane p r c s) = fromProd @cst (p, r, c, s)
  prod = prod @cst @(Position, Direction, Color, Float)

instance Lift Exp Plane where
  type Plain Plane = Plane
  lift = constant

-- ** Light
instance Elt Light where
  type EltRepr Light = EltRepr (Position, Color)
  eltType = eltType @(Position, Color)
  toElt t = let (p, c) = toElt t in Light p c
  fromElt (Light p c) = fromElt (p, c)

instance (cst (V3 Float)) => IsProduct cst Light where
  type ProdRepr Light = ProdRepr (Position, Color)
  toProd t = let (p, c) = toProd @cst t in Light p c
  fromProd (Light p c) = fromProd @cst (p, c)
  prod = prod @cst @(Position, Color)

instance Lift Exp Light where
  type Plain Light = Light
  lift = constant

-- ** Ray
instance Elt Ray where
  type EltRepr Ray = EltRepr (Position, Direction)
  eltType = eltType @(Position, Direction)
  toElt t = let (p, c) = toElt t in Ray p c
  fromElt (Ray p c) = fromElt (p, c)

instance (cst (V3 Float)) => IsProduct cst Ray where
  type ProdRepr Ray = ProdRepr (Position, Direction)
  toProd t = let (p, c) = toProd @cst t in Ray p c
  fromProd (Ray p c) = fromProd @cst (p, c)
  prod = prod @cst @(Position, Direction)

instance Lift Exp Ray where
  type Plain Ray = Ray
  lift = constant

-- ** Camera
instance Elt Camera where
  type EltRepr Camera = EltRepr (Position, Direction, Int)
  eltType = eltType @(Position, Direction, Int)
  toElt t = let (p, r, f) = toElt t in Camera p r f
  fromElt (Camera p r f) = fromElt (p, r, f)

instance (cst Int, cst (V3 Float)) => IsProduct cst Camera where
  type ProdRepr Camera = ProdRepr (Position, Direction, Int)
  toProd t = let (p, r, f) = toProd @cst t in Camera p r f
  fromProd (Camera p r f) = fromProd @cst (p, r, f)
  prod = prod @cst @(Position, Direction, Int)

instance Lift Exp Camera where
  type Plain Camera = Camera
  lift = constant
