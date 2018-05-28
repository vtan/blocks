module App.GameState where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Editor as Editor
import qualified App.Level as Level
import qualified App.NonEmptyZipper as NonEmptyZipper
import qualified App.Rect as Rect
import qualified SDL
import qualified SDL.Internal.Numbered

import App.Block (Block(..))
import App.Camera (Camera)
import App.Editor (Editor)
import App.Level (Level)
import App.NonEmptyZipper (NonEmptyZipper(..))

data GameState = GameState
  { levels :: NonEmptyZipper Level
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
levelWon GameState{ blockById, levels } =
  let collectorRect = levels ^. #current & Level.collectorRect 
  in blockById
    & toList
    & filter (view #behavior >>> has (_Ctor @"Collectable"))
    & all (view #rect >>> Rect.intersects collectorRect)

-- TODO use a `Traversal' GameState (EditorState, Level)` so the Editor module can "change"
-- currentLevel of the GameState instead of having a redundant level which needs to be applied
-- at various events?
applyEditorChanges :: Editor -> GameState -> GameState
applyEditorChanges editor gs =
  let changedLevel = editor ^. #level
  in gs
    & #levels . #current .~ changedLevel
    & #blockById .~ changedLevel ^. #blockById

goNextLevel :: GameState -> GameState
goNextLevel gs@GameState{ levels } =
  case NonEmptyZipper.next levels of
    Just levels' -> gs
      & #levels .~ levels'
      & #blockById .~ levels' ^. #current . #blockById
      & #editor . _Just .~ Editor.fromLevel (levels' ^. #current)
    Nothing -> gs

goPrevLevel :: GameState -> GameState
goPrevLevel gs@GameState{ levels } =
  case NonEmptyZipper.prev levels of
    Just levels' -> gs
      & #levels .~ levels'
      & #blockById .~ levels' ^. #current . #blockById
      & #editor . _Just .~ Editor.fromLevel (levels' ^. #current)
    Nothing -> gs

addLevel :: GameState -> GameState
addLevel gs@GameState{ levels } =
  let newLevel = Level.empty
      levels' = NonEmptyZipper.pushAfter newLevel levels
  in gs
    & #levels .~ levels'
    & #blockById .~ newLevel ^. #blockById
    & #editor . _Just .~ Editor.fromLevel newLevel

removeLevel :: GameState -> GameState
removeLevel gs@GameState{ levels } =
  let levels' = (NonEmptyZipper.shiftFromBefore levels <|> NonEmptyZipper.shiftFromAfter levels)
        & fromMaybe (NonEmptyZipper.singleton Level.empty)
  in gs
    & #levels .~ levels'
    & #blockById .~ levels' ^. #current . #blockById
    & #editor . _Just .~ Editor.fromLevel (levels' ^. #current)

initial :: GameState
initial = GameState
  { levels = NonEmptyZipper.fromNonEmpty [Level.initial, Level.empty]
  , blockById = view #blockById Level.initial
  , currentAnimation = Nothing
  , editor = Nothing
  , camera = Camera.initial
  , totalTime = 0
  , keyModifier = SDL.Internal.Numbered.fromNumber 0
  , quit = False
  }
