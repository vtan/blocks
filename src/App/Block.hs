module App.Block where

import App.Prelude

import App.Rect (Rect)

data Block a = Block
  { uid :: Int
  , rect :: Rect a
  , orientation :: V2 Int
  , behavior :: Behavior
  }
  deriving (Show, Generic, Functor)

data Behavior
  = Static
  | Pushable
  | Collectable
  | Movable
  | Flippable { flipped :: Bool }
  deriving (Show, Generic)

eqUid :: Block a -> Block a -> Bool
eqUid = (==) `on` view #uid