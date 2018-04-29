module App.GameState where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera
import qualified App.Rect as Rect

import App.Block (Block(..))
import App.Camera (Camera)
import App.Rect (Rect(..))

data GameState = GameState
  { _blockById :: IntMap Block
  , _currentAnimation :: Maybe Animation
  , _camera :: Camera Int
  , _totalTime :: Float
  , _quit :: Bool
  }
  deriving (Show, Generic)

data Animation = Animation
  { _start :: Float
  , _end :: Float
  , _movingBlocksById :: [(Block, V2 Int)]
  , _otherBlocksById :: [Block]
  , _after :: GameState
  }
  deriving (Show, Generic)

findBlockAt :: GameState -> V2 Int -> Maybe Block
findBlockAt GameState{ _blockById = blocks } p =
  blocks & find (\b -> Rect.contains (view #_rect b) p)

initial :: GameState
initial = GameState
  { _blockById =
    [ (0, Block 0 (Rect (V2 4 0) (V2 1 1)) (Block.Flippable (V2 1 0) False))
    , (1, Block 1 (Rect (V2 6 1) (V2 1 1)) Block.Pushable)
    , (2, Block 2 (Rect (V2 5 0) (V2 1 2)) Block.Pushable)
    , (3, Block 3 (Rect (V2 7 0) (V2 1 1)) Block.Static)
    ]
  , _currentAnimation = Nothing
  , _camera = Camera.initial
  , _totalTime = 0
  , _quit = False
  }
