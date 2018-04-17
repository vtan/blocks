module App.GameState where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera

data GameState = GameState
  { _blockById :: IntMap Block.Block
  , _camera :: Camera.Camera
  , _quit :: Bool
  }
  deriving (Show, Generic)

findBlockAt :: GameState -> V2 Int -> Maybe Block.Block
findBlockAt GameState{ _blockById = blocks } p =
  blocks & find (\b -> Block.contains b p)

initial :: GameState
initial = GameState
  { _blockById =
    [ (0, Block.Block 0 (V2 0 0) (V2 1 1) (Block.Movable (V2 1 0)))
    , (1, Block.Block 1 (V2 6 1) (V2 1 1) Block.Pushable)
    , (2, Block.Block 2 (V2 5 0) (V2 1 2) Block.Pushable)
    , (3, Block.Block 3 (V2 7 0) (V2 1 1) Block.Static)
    ]
  , _camera = Camera.initial
  , _quit = False
  }
