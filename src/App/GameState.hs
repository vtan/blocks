module App.GameState where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Editor as Editor
import qualified App.Level as Level
import qualified App.Rect as Rect
import qualified Data.List.NonEmpty as NonEmpty
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

-- TODO use a `Traversal' GameState (EditorState, Level)` so the Editor module can "change"
-- currentLevel of the GameState instead of having a redundant level which needs to be applied
-- at various events?
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

addLevel :: GameState -> GameState
addLevel gs@GameState{ levels, currentLevelIx } =
  let newLevel = Level.empty
      newIx = currentLevelIx + 1
      (levelsBeforeNew, levelsAfterNew) = NonEmpty.splitAt newIx levels
      levels' = case NonEmpty.nonEmpty (levelsBeforeNew ++ [newLevel] ++ levelsAfterNew) of
        Just ne -> ne
        Nothing -> newLevel :| []
  in gs
    & #levels .~ levels'
    & #currentLevel .~ newLevel
    & #currentLevelIx .~ newIx
    & #blockById .~ newLevel ^. #blockById
    & #editor . _Just .~ Editor.fromLevel newLevel

removeLevel :: GameState -> GameState
removeLevel gs@GameState{ levels, currentLevelIx } =
  let (levelsBefore, levelsAfter) = NonEmpty.splitAt currentLevelIx levels
      levels' = case NonEmpty.nonEmpty (levelsBefore ++ drop 1 levelsAfter) of
        Just ne -> ne
        Nothing -> Level.empty :| []
      (currentLevel', currentLevelIx')
        | currentLevelIx == 0 = (NonEmpty.head levels', 0)
        | otherwise = 
            ( NonEmpty.nonEmpty levelsBefore
                & fmap NonEmpty.last
                & fromMaybe Level.empty
            , currentLevelIx - 1
            )
  in gs
    & #levels .~ levels'
    & #currentLevel .~ currentLevel'
    & #currentLevelIx .~ currentLevelIx'
    & #blockById .~ currentLevel' ^. #blockById
    & #editor . _Just .~ Editor.fromLevel currentLevel'
-- TODO use a `Lens' GameState ([Level], NonEmptyLevel)` to reduce invariant maintenance boilerplate?

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
