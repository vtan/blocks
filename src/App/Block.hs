module App.Block where

import App.Prelude

import App.Rect (Rect)

data Block a = Block
  { _id :: Int
  , _rect :: Rect a
  , _behavior :: Behavior
  }
  deriving (Show, Generic, Functor)

data Behavior
  = Static
  | Movable { _direction :: V2 Int }
  | Flippable { _direction :: V2 Int, _flipped :: Bool }
  | Pushable
  deriving (Show, Generic)

eqId :: Block a -> Block a -> Bool
eqId = (==) `on` view #_id