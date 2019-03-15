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

-- | Any ray that is cast through the scene. This is defined as a type alias as
-- the 'Ray' has to be polymorphic in order to to be able to lift a @Ray (Exp
-- (V3 Float)) (Exp (V3 Float))@ into a @Exp (Ray (V3 Float) (V3 Float))@.
type Ray = Ray' Float
data Ray' a = Ray
  { _rayOrigin :: V3 a
  , _rayDirection :: V3 a
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

class HasPosition t a | t -> a where
  position :: Getter (Exp t) (Exp a)
instance HasPosition Sphere Position where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t
instance HasPosition Plane Position where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx (SuccTupIdx ZeroTupIdx)) `Prj` t
instance HasPosition Light Position where
  position = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t
instance HasPosition Camera Position where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t

class HasColor t a | t -> a where
  color :: Getter (Exp t) (Exp a)
instance HasColor Sphere Color where
  color = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t
instance HasColor Plane Color where
  color = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t
instance HasColor Light Color where
  color = to $ \t -> Exp $ ZeroTupIdx `Prj` t

class HasSpecularity t a | t -> a where
  specularity :: Getter (Exp t) (Exp a)
instance HasSpecularity Sphere Float where
  specularity = to $ \t -> Exp $ ZeroTupIdx `Prj` t
instance HasSpecularity Plane Float where
  specularity = to $ \t -> Exp $ ZeroTupIdx `Prj` t

class HasDirection t a | t -> a where
  direction :: Getter (Exp t) (Exp a)
instance HasDirection Plane Direction where
  direction = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
instance Elt a => HasDirection (Ray' a) (V3 a) where
  direction = to $ \t -> Exp $ ZeroTupIdx `Prj` t
instance HasDirection Camera Direction where
  direction = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t

radius :: Getter (Exp Sphere) (Exp Float)
radius = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t

origin :: Elt a => Getter (Exp (Ray' a)) (Exp (V3 a))
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
instance Elt a => Elt (Ray' a) where
  type EltRepr (Ray' a) = EltRepr (V3 a, V3 a)
  eltType = eltType @(V3 a, V3 a)
  toElt t = let (p, c) = toElt t in Ray p c
  fromElt (Ray p c) = fromElt (p, c)

instance (cst (V3 a)) => IsProduct cst (Ray' a) where
  type ProdRepr (Ray' a) = ProdRepr (V3 a, V3 a)
  toProd t = let (p, c) = toProd @cst t in Ray p c
  fromProd (Ray p c) = fromProd @cst (p, c)
  prod = prod @cst @(V3 a, V3 a)

instance Elt a => Lift Exp (Ray' a) where
  type Plain (Ray' a) = (Ray' a)
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
