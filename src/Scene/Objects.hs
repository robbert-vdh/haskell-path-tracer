module Scene.Objects where

import Data.Typeable
import Data.Array.Accelerate as A
import Data.Array.Accelerate.Linear as A
import qualified Prelude

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
