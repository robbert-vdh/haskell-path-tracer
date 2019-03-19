{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
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

data Scene = Scene
  { _sceneSpheres :: A.Vector Sphere
  , _scenePlanes :: A.Vector Plane
  , _sceneLights :: A.Vector Light
  } deriving (Typeable)

data Sphere = Sphere
  { _spherePosition :: Position
  , _sphereRadius :: Float
  , _sphereColor :: Color
  , _sphereSpecularity :: Float
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt)

data Plane = Plane
  { _planePosition :: Position
  , _planeDirection :: Direction
  , _planeColor :: Color
  , _planeSpecularity :: Float
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt)

data Light = Light
  { _lightPosition :: Position
  , _lightColor :: Color
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt)

-- | Any ray that is cast through the scene. This is defined as a type alias as
-- the 'Ray' has to be polymorphic in order to to be able to lift a @Ray (Exp
-- (V3 Float)) (Exp (V3 Float))@ into a @Exp (Ray (V3 Float) (V3 Float))@.
type RayF = Ray Float
data Ray a = Ray
  { _rayOrigin :: V3 a
  , _rayDirection :: V3 a
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt)

data Camera = Camera
  { _cameraPosition :: Position
  , _cameraDirection :: Direction
  , _cameraFov :: Int
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt)

-- * Lenses
--
-- Since Sphere, Plane and Light do not have a type parameter we can't make use
-- 'liftLens' or `unlift`, so we'll just define some simple getters ourselves.
--
-- ** TODO: Decide on whether we actually need these now that pattern synonyms
--          are a thing

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
instance Elt a => HasDirection (Ray a) (V3 a) where
  direction = to $ \t -> Exp $ ZeroTupIdx `Prj` t
instance HasDirection Camera Direction where
  direction = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t

radius :: Getter (Exp Sphere) (Exp Float)
radius = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t

origin :: Elt a => Getter (Exp (Ray a)) (Exp (V3 a))
origin = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t

fov :: Getter (Exp Camera) (Exp Int)
fov = to $ \t -> Exp $ ZeroTupIdx `Prj` t

-- * Pattern synonyms
--
-- See the documentation for 'Data.Array.Accelerate' for more information about
-- these.

pattern Ray' :: Elt a => Exp (V3 a) -> Exp (V3 a) -> Exp (Ray a)
pattern Ray' o d = Pattern (o, d)

pattern Camera' :: Exp Position -> Exp Direction -> Exp Int -> Exp Camera
pattern Camera' p d f = Pattern (p, d, f)

-- * Instances
--
-- For our data types we can simply derive most of these or use the default
-- implementations.

-- ** Sphere

instance (cst Float, cst (V3 Float)) => IsProduct cst Sphere

instance Lift Exp Sphere where
  type Plain Sphere = Sphere
  lift = constant

-- ** Plane

instance (cst Float, cst (V3 Float)) => IsProduct cst Plane

instance Lift Exp Plane where
  type Plain Plane = Plane
  lift = constant

-- ** Light

instance (cst (V3 Float)) => IsProduct cst Light

instance Lift Exp Light where
  type Plain Light = Light
  lift = constant

-- ** Ray

instance (cst (V3 a)) => IsProduct cst (Ray a)

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Ray a) where
  type Plain (Ray a) = Ray (Plain a)
  lift (Ray o d) = Exp $ Tuple $ NilTup `SnocTup` lift o `SnocTup` lift d

-- ** Camera

instance (cst Int, cst (V3 Float)) => IsProduct cst Camera

instance Lift Exp Camera where
  type Plain Camera = Camera
  lift = constant
