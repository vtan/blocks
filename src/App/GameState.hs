module App.GameState where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera

data GameState = GameState
  { _blockById :: IntMap Block.Block
  , _blockIdByOrigin :: HashMap (V2 Int) Int
  , _levelExtent :: V2 Int
  , _camera :: Camera.Camera
  , _quit :: Bool
  }
  deriving (Show, Generic)

initial :: GameState
initial = GameState
  { _blockById =
    [ (0, Block.Block 0 (V2 0 0) (V2 1 1) (Block.Movable (V2 1 0)))
    , (1, Block.Block 1 (V2 0 2) (V2 1 1) Block.Static)
    ]
  , _blockIdByOrigin =
    [ (V2 0 0, 0)
    , (V2 0 2, 1)
    ]
  , _levelExtent = V2 5 5
  , _camera = Camera.initial
  , _quit = False
  }
