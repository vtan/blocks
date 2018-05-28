module App.GameState where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Editor as Editor
import qualified App.Level as Level
import qualified App.Rect as Rect
import qualified SDL
import qualified SDL.Internal.Numbered

import App.Block (Block(..))
import App.Camera (Camera)
import App.Editor (Editor)
import App.Level (Level)

data GameState = GameState
  { levels :: NonEmpty Level
  , currentLevel :: Level
  , currentLevelIx :: Int
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

applyEditorChanges :: Editor -> GameState -> GameState
applyEditorChanges editor gs@GameState { currentLevelIx } =
  let changedLevel = editor ^. #level
  in gs
    & #levels . ix currentLevelIx .~ changedLevel
    & #currentLevel .~ changedLevel
    & #blockById .~ changedLevel ^. #blockById

changeLevel :: Int -> GameState -> GameState
changeLevel ixDiff gs@GameState{ levels, currentLevelIx } =
  let nextIx = currentLevelIx + ixDiff
  in case levels ^? ix nextIx of
    Just nextLevel -> gs
      & #currentLevel .~ nextLevel
      & #currentLevelIx .~ nextIx
      & #blockById .~ nextLevel ^. #blockById
      & #editor . _Just .~ Editor.fromLevel nextLevel
    Nothing -> gs

initial :: GameState
initial = GameState
  { levels = [Level.initial, Level.empty]
  , currentLevel = Level.initial
  , currentLevelIx = 0
  , blockById = view #blockById Level.initial
  , currentAnimation = Nothing
  , editor = Nothing
  , camera = Camera.initial
  , totalTime = 0
  , keyModifier = SDL.Internal.Numbered.fromNumber 0
  , quit = False
  }
