module App.Level where

import App.Prelude

import qualified App.Block as Block

import App.Block (Block(..))
import App.Rect (Rect(..))

data Level = Level
  { _blockById :: IntMap (Block Int) -- TODO store as a Vector or List instead?
  , _bounds :: Rect Int
  }
  deriving (Show, Generic)

initial :: Level
initial = Level
  { _blockById =
    [ (0, Block 0 (Rect (V2 4 0) (V2 1 1)) (Block.Flippable (V2 1 0) False))
    , (1, Block 1 (Rect (V2 6 1) (V2 1 1)) Block.Pushable)
    , (2, Block 2 (Rect (V2 5 0) (V2 1 2)) Block.Pushable)
    , (3, Block 3 (Rect (V2 7 0) (V2 1 1)) Block.Static)
    , (4, Block 4 (Rect (V2 4 2) (V2 1 1)) (Block.Movable (V2 1 0)))
    ]
  , _bounds = Rect (V2 3 (-1)) (V2 6 4)
  }
