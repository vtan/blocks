module App.GameState where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Level as Level
import qualified App.Rect as Rect

import App.Block (Block(..))
import App.Camera (Camera)
import App.Editor (Editor)
import App.Level (Level)

data GameState = GameState
  { _currentLevel :: Level
  , _blockById :: IntMap (Block Int)
  , _currentAnimation :: Maybe Animation
  , _editor :: Maybe Editor
  , _camera :: Camera Int
  , _totalTime :: Float
  , _quit :: Bool
  }
  deriving (Show, Generic)

data Animation = Animation
  { _start :: Float
  , _end :: Float
  , _movingBlocksById :: [(Block Int, V2 Int)]
  , _otherBlocksById :: [Block Int]
  , _after :: GameState
  }
  deriving (Show, Generic)

findBlockAt :: GameState -> V2 Int -> Maybe (Block Int)
findBlockAt GameState{ _blockById = blocks } p =
  blocks & find (\b -> Rect.contains (view #_rect b) p)

initial :: GameState
initial = GameState
  { _currentLevel = Level.initial
  , _blockById = view #_blockById Level.initial
  , _currentAnimation = Nothing
  , _editor = Nothing
  , _camera = Camera.initial
  , _totalTime = 0
  , _quit = False
  }
