module App.Block where

import App.Prelude

import App.Rect (Rect)

data Block = Block
  { _id :: Int
  , _rect :: Rect Int
  , _behavior :: Behavior
  }
  deriving (Show, Generic)

data Behavior
  = Static
  | Movable { _direction :: V2 Int }
  | Flippable { _direction :: V2 Int, _flipped :: Bool }
  | Pushable
  deriving (Show, Generic)

eqId :: Block -> Block -> Bool
eqId = (==) `on` view #_id