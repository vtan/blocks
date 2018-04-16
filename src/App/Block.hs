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
  | Pushable
  deriving (Show, Generic)

contains :: Block -> V2 Int -> Bool
contains Block{ _origin = V2 bx by, _extent = V2 bw bh } (V2 px py) =
  px >= bx && px < bx + bw && py >= by && py < by + bh
