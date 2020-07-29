{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Scene.Objects where

import           Data.Array.Accelerate         as A
import           Data.Array.Accelerate.Array.Sugar
import           Data.Array.Accelerate.Control.Lens
                                         hiding ( Const )
import           Data.Array.Accelerate.Linear  as A
import           Data.Typeable

import           Prelude                        ( (<$>) )
import qualified Prelude                       as P

-- | The result of a computation. This is an array of accumulated color values
-- for every pixel along with the RNG seed that should be used for the next
-- iteration. When rendering we simply divide the color value by the number of
-- iterations run to get an average pixel value (or in cool people's terms,
-- we're integrating over the illuminance at a single point in the scene).
--
-- The RNG seeds should be reseeded periodically since we're using very
-- rudimentary PRNG algorithms.
type RenderResult = Matrix (Color, Word32)

-- * Objects

type Point = V3 Float
type Direction = V3 Float
type Color = V3 Float
type Noraml = (Point, Direction)

data Scene = Scene
  { _sceneSpheres :: [Exp Sphere]
  , _scenePlanes  :: [Exp Plane]
  }
  deriving Typeable

data Camera = Camera
  { _cameraPosition :: Point
  -- | The camera's rotation expressed in @(roll, pitch, yaw)@ Euler angles.
  , _cameraRotation :: Direction
  -- | The camera's horizontal field of view in degrees.
  , _cameraFov      :: Int
  }
  deriving (P.Eq, Show, Generic, Elt)

data Brdf
  = Diffuse {-# UNPACK #-} Float
  | Glossy {-# UNPACK #-} Float
  deriving (P.Eq, Show, Typeable)

data Material = Material
  { _materialColor       :: {-# UNPACK #-} Color
  , _materialIlluminance :: {-# UNPACK #-} Float
  , _materialBrdf        :: Brdf
  }
  deriving (P.Eq, Show, Generic, Elt)

data Plane = Plane
  { _planePosition  :: {-# UNPACK #-} Point
  , _planeDirection :: {-# UNPACK #-} Direction
  , _planeMaterial  :: Material
  }
  deriving (P.Eq, Show, Generic, Elt)

-- | Any ray that is cast through the scene. This is defined as a type alias as
-- the 'Ray' has to be polymorphic in order to to be able to lift a @Ray (Exp
-- (V3 Float)) (Exp (V3 Float))@ into a @Exp (Ray (V3 Float) (V3 Float))@.
data Ray a = Ray
  { _rayOrigin    :: V3 a
  , _rayDirection :: V3 a
  }
  deriving (P.Eq, Show, Generic, Elt)
type RayF = Ray Float

-- | A normal and the point from which that normal was calculated (i.e. an
-- intersection).
type NormalP = RayF

data Sphere = Sphere
  { _spherePosition :: {-# UNPACK #-} Point
  , _sphereRadius   :: {-# UNPACK #-} Float
  , _sphereMaterial :: {-# UNPACK #-} Material
  }
  deriving (P.Eq, Show, Generic, Elt)

-- * Pattern synonyms
--
-- See the documentation for 'Data.Array.Accelerate' for more information about
-- these.

pattern Camera_ :: Exp Point -> Exp Direction -> Exp Int -> Exp Camera
pattern Camera_ p d f = Pattern (p, d, f)

pattern Material_ :: Exp Color -> Exp Float -> Exp Brdf -> Exp Material
pattern Material_ c i b = Pattern (c, i, b)

pattern Plane_ :: Exp Point -> Exp Direction -> Exp Material -> Exp Plane
pattern Plane_ p d m = Pattern (p, d, m)

pattern Ray_ :: Elt a => Exp (V3 a) -> Exp (V3 a) -> Exp (Ray a)
pattern Ray_ o d = Pattern (o, d)

pattern Sphere_ :: Exp Point -> Exp Float -> Exp Material -> Exp Sphere
pattern Sphere_ p r m = Pattern (p, r, m)

{-# COMPLETE Camera_ #-}
{-# COMPLETE Material_ #-}
{-# COMPLETE Plane_ #-}
{-# COMPLETE Ray_ #-}
{-# COMPLETE Sphere_ #-}

-- * Instances

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
  lift (Ray o d) = Ray_ (lift o) (lift d)

instance Lift Exp Sphere where
  type Plain Sphere = Sphere
  lift = constant

-- ** BRDF
--
-- We can't derive instances for 'Brdf' automatically as Accelerate does not yet
-- support sum types. To work around this, we simply use define our own tagged
-- unions as tuples.
--
-- Support for sum types is coming very soon though! :tada:
-- See https://github.com/AccelerateHS/accelerate/pull/460

instance Elt Brdf where
  type EltRepr Brdf = EltRepr (Bool, Float)
  eltType = eltType @(Bool, Float)
  toElt t = case toElt t of
    (False, p) -> Diffuse p
    (True , p) -> Glossy p
  fromElt (Diffuse p) = fromElt (False, p)
  fromElt (Glossy  p) = fromElt (True, p)

instance Lift Exp Brdf where
  type Plain Brdf = Brdf
  lift = constant

-- * Lenses
--
-- Since Sphere, Plane and Light do not have a type parameter we can't make use
-- 'liftLens' or `unlift`, so we'll just define some simple getters ourselves.

makeFields ''Scene

-- TODO: Check if there is a new, better way to define these lenses

class HasBrdf t a | t -> a where
  brdf :: Getter (Exp t) (Exp a)
instance HasBrdf Material Brdf where
  brdf = to $ \(Material_ _ _ b) -> b

class HasColor t a | t -> a where
  color :: Getter (Exp t) (Exp a)
instance HasColor Material Color where
  color = to $ \(Material_ c _ _) -> c

class HasDirection t a | t -> a where
  direction :: Getter (Exp t) (Exp a)
instance HasDirection Plane Direction where
  direction = to $ \(Plane_ _ d _) -> d
instance Elt a => HasDirection (Ray a) (V3 a) where
  direction = to $ \(Ray_ _ d) -> d

class HasFov t a | t -> a where
  fov :: Getter (Exp t) (Exp a)
instance HasFov Camera Int where
  fov = to $ \(Camera_ _ _ f) -> f

class HasIlluminance t a | t -> a where
  illuminance :: Getter (Exp t) (Exp a)
instance HasIlluminance Material Float where
  illuminance = to $ \(Material_ _ i _) -> i

class HasMaterial t a | t -> a where
  material :: Getter (Exp t) (Exp a)
instance HasMaterial Plane Material where
  material = to $ \(Plane_ _ _ m) -> m
instance HasMaterial Sphere Material where
  material = to $ \(Sphere_ _ _ m) -> m

class HasOrigin t a | t -> a where
  origin :: Getter (Exp t) (Exp a)
instance Elt a => HasOrigin (Ray a) (V3 a) where
  origin = to $ \(Ray_ o _) -> o

class HasPosition t a | t -> a where
  position :: Getter (Exp t) (Exp a)
instance HasPosition Camera Point where
  position = to $ \(Camera_ p _ _) -> p
instance HasPosition Plane Point where
  position = to $ \(Plane_ p _ _) -> p
instance HasPosition Sphere Point where
  position = to $ \(Sphere_ p _ _) -> p

class HasRadius t a | t -> a where
  radius :: Getter (Exp t) (Exp a)
instance HasRadius Sphere Float where
  radius = to $ \(Sphere_ _ r _) -> r

class HasRotation t a | t -> a where
  rotation :: Getter (Exp t) (Exp a)
instance HasRotation Camera Direction where
  rotation = to $ \(Camera_ _ d _) -> d

-- These two lenses are used for camera movement. As they're the only two lenses
-- we need that have setters and are not lifted to 'Exp's we'll dimply define
-- them manually.
-- TODO: Maybe update these too

rotation' :: Lens' Camera Direction
rotation' f c@Camera { _cameraRotation = rot } =
  (\x -> c { _cameraRotation = x }) <$> f rot
position' :: Lens' Camera Point
position' f c@Camera { _cameraPosition = pos } =
  (\x -> c { _cameraPosition = x }) <$> f pos

-- ** BRDF
--
-- Accelerate does not yet support sum types, but we can simply pattern match on
-- the product representation to get the same effect.

pattern Brdf_ :: Exp Bool -> Exp Float -> Exp Brdf
pattern Brdf_ t p = Pattern (t, p)

-- These sadly don't work, but I'll leave them in until they somehow do
pattern Diffuse_ :: Exp Float -> Exp Brdf
pattern Diffuse_ p = Brdf_ False_ p

pattern Glossy_ :: Exp Float -> Exp Brdf
pattern Glossy_ p = Brdf_ True_ p

isDiffuse, isGlossy :: Exp Brdf -> Exp Bool
isDiffuse (Brdf_ t _) = t == constant False
isDiffuse _           = constant False
isGlossy (Brdf_ t _) = t == constant True
isGlossy _           = constant False
