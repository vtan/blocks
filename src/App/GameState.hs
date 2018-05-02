module App.GameState where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera
import qualified App.Rect as Rect

import App.Block (Block(..))
import App.Camera (Camera)
import App.Rect (Rect(..))

data GameState = GameState
  { _blockById :: IntMap (Block Int)
  , _levelBounds :: Rect Int
  , _currentAnimation :: Maybe Animation
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
  { _blockById =
    [ (0, Block 0 (Rect (V2 4 0) (V2 1 1)) (Block.Flippable (V2 1 0) False))
    , (1, Block 1 (Rect (V2 6 1) (V2 1 1)) Block.Pushable)
    , (2, Block 2 (Rect (V2 5 0) (V2 1 2)) Block.Pushable)
    , (3, Block 3 (Rect (V2 7 0) (V2 1 1)) Block.Static)
    , (4, Block 4 (Rect (V2 4 2) (V2 1 1)) (Block.Movable (V2 1 0)))
    ]
  , _levelBounds = Rect (V2 3 (-1)) (V2 6 4)
  , _currentAnimation = Nothing
  , _camera = Camera.initial
  , _totalTime = 0
  , _quit = False
  }
