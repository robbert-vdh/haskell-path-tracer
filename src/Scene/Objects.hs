{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Scene.Objects where

-- Accelerate
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Linear.Metric
import Data.Array.Accelerate.Linear.V3
import Data.Array.Accelerate.Linear.Vector

-- standard library
import Data.Typeable
import qualified Prelude as P

-- Types
type Position     = V3 Float
type Direction    = V3 Float
type Colour       = V3 Float
type Noraml = (Position, Direction)

data Sphere = Sphere Position Float Colour Float
    deriving (P.Eq, P.Show, Typeable)

data Plane = Plane Position Direction Colour Float
    deriving (P.Eq, P.Show, Typeable)
