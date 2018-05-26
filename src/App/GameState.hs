module App.GameState where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Level as Level
import qualified App.Rect as Rect
import qualified SDL
import qualified SDL.Internal.Numbered

import App.Block (Block(..))
import App.Camera (Camera)
import App.Editor (Editor)
import App.Level (Level)

data GameState = GameState
  { currentLevel :: Level
  , blockById :: IntMap (Block Int)
  , currentAnimation :: Maybe Animation
  , editor :: Maybe Editor
  , camera :: Camera Int
  , totalTime :: Float
  , keyModifier :: SDL.KeyModifier
  , quit :: Bool
  }
  deriving (Show, Generic)

data Animation = Animation
  { start :: Float
  , end :: Float
  , movingBlocksById :: [(Block Int, V2 Int)]
  , otherBlocksById :: [Block Int]
  , after :: GameState
  }
  deriving (Show, Generic)

findBlockAt :: GameState -> V2 Int -> Maybe (Block Int)
findBlockAt GameState{ blockById = blocks } p =
  blocks & find (\b -> Rect.contains (view #rect b) p)

levelWon :: GameState -> Bool
levelWon GameState{ blockById, currentLevel } =
  let collectorRect = Level.collectorRect currentLevel
  in blockById
    & toList
    & filter (view #behavior >>> has (_Ctor @"Collectable"))
    & all (view #rect >>> Rect.intersects collectorRect)

initial :: GameState
initial = GameState
  { currentLevel = Level.initial
  , blockById = view #blockById Level.initial
  , currentAnimation = Nothing
  , editor = Nothing
  , camera = Camera.initial
  , totalTime = 0
  , keyModifier = SDL.Internal.Numbered.fromNumber 0
  , quit = False
  }
