module App.Block where

import App.Prelude

data Block = Block
  { _id :: Int
  , _origin :: V2 Int
  , _extent :: V2 Int
  , _behavior :: Behavior
  }
  deriving (Show, Generic)

data Behavior
  = Static
  | Movable { _direction :: V2 Int }
  deriving (Show, Generic)
