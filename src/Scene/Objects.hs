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
  { _sceneSpheres :: [Sphere]
  , _scenePlanes :: [Plane]
  } deriving (Typeable)

data Camera = Camera
  { _cameraPosition :: Position
  , _cameraDirection :: Direction
  , _cameraFov :: Int
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt, IsProduct cst)

data Material = Material
  { _materialColor :: Color
  , _materialSpecularity :: Float
  , _materialIlluminance :: Float
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt, IsProduct cst)

data Plane = Plane
  { _planePosition :: Position
  , _planeDirection :: Direction
  , _planeMaterial :: Material
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt, IsProduct cst)

-- | Any ray that is cast through the scene. This is defined as a type alias as
-- the 'Ray' has to be polymorphic in order to to be able to lift a @Ray (Exp
-- (V3 Float)) (Exp (V3 Float))@ into a @Exp (Ray (V3 Float) (V3 Float))@.
type Normal = RayF
type RayF = Ray Float
data Ray a = Ray
  { _rayOrigin :: V3 a
  , _rayDirection :: V3 a
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt, IsProduct cst)

data Sphere = Sphere
  { _spherePosition :: Position
  , _sphereRadius :: Float
  , _sphereMaterial :: Material
  } deriving (Prelude.Eq, Show, Typeable, Generic, Elt, IsProduct cst)

-- * Lenses
--
-- Since Sphere, Plane and Light do not have a type parameter we can't make use
-- 'liftLens' or `unlift`, so we'll just define some simple getters ourselves.

makeFields ''Scene

class HasColor t a | t -> a where
  color :: Getter (Exp t) (Exp a)
instance HasColor Material Color where
  color = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t

class HasDirection t a | t -> a where
  direction :: Getter (Exp t) (Exp a)
instance HasDirection Camera Direction where
  direction = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t
instance HasDirection Plane Direction where
  direction = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
instance Elt a => HasDirection (Ray a) (V3 a) where
  direction = to $ \t -> Exp $ ZeroTupIdx `Prj` t

class HasFov t a | t -> a where
  fov :: Getter (Exp t) (Exp a)
instance HasFov Camera Int where
  fov = to $ \t -> Exp $ ZeroTupIdx `Prj` t

class HasIlluminance t a | t -> a where
  illuminance :: Getter (Exp t) (Exp a)
instance HasIlluminance Material Float where
  illuminance = to $ \t -> Exp $ ZeroTupIdx `Prj` t

class HasMaterial t a | t -> a where
  material :: Getter (Exp t) (Exp a)
instance HasMaterial Plane Material where
  material = to $ \t -> Exp $ ZeroTupIdx `Prj` t
instance HasMaterial Sphere Material where
  material = to $ \t -> Exp $ ZeroTupIdx `Prj` t

class HasOrigin t a | t -> a where
  origin :: Getter (Exp t) (Exp a)
instance Elt a => HasOrigin (Ray a) (V3 a) where
  origin = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t

class HasPosition t a | t -> a where
  position :: Getter (Exp t) (Exp a)
instance HasPosition Camera Position where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
instance HasPosition Plane Position where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t
instance HasPosition Sphere Position where
  position = to $ \t -> Exp $ SuccTupIdx (SuccTupIdx ZeroTupIdx) `Prj` t

class HasRadius t a | t -> a where
  radius :: Getter (Exp t) (Exp a)
instance HasRadius Sphere Float where
  radius = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t

class HasSpecularity t a | t -> a where
  specularity :: Getter (Exp t) (Exp a)
instance HasSpecularity Material Float where
  specularity = to $ \t -> Exp $ SuccTupIdx ZeroTupIdx `Prj` t

-- * Pattern synonyms
--
-- See the documentation for 'Data.Array.Accelerate' for more information about
-- these.

pattern Camera' :: Exp Position -> Exp Direction -> Exp Int -> Exp Camera
pattern Camera' p d f = Pattern (p, d, f)

pattern Material' :: Exp Color -> Exp Float -> Exp Float -> Exp Material
pattern Material' c s i = Pattern (c, s, i)

pattern Plane' :: Exp Position -> Exp Direction -> Exp Material -> Exp Plane
pattern Plane' p d m = Pattern (p, d, m)

pattern Ray' :: Elt a => Exp (V3 a) -> Exp (V3 a) -> Exp (Ray a)
pattern Ray' o d = Pattern (o, d)

pattern Sphere' :: Exp Position -> Exp Float -> Exp Material -> Exp Sphere
pattern Sphere' p r m = Pattern (p, r, m)

-- * Instances
--
-- The implementations for 'Elt' and 'IsProduct' are derived through 'Generic'.

instance Lift Exp Camera where
  type Plain Camera = Camera
  lift = constant

instance Lift Exp Plane where
  type Plain Plane = Plane
  lift = constant

instance Lift Exp Material where
  type Plain Material = Material
  lift = constant

instance (Lift Exp a, Elt (Plain a)) => Lift Exp (Ray a) where
  type Plain (Ray a) = Ray (Plain a)
  lift (Ray o d) = Exp $ Tuple $ NilTup `SnocTup` lift o `SnocTup` lift d

instance Lift Exp Sphere where
  type Plain Sphere = Sphere
  lift = constant
