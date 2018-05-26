module App.Editor where

import App.Prelude

import qualified App.Block as Block
import qualified App.Level as Level
import qualified App.Rect as Rect
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet

import App.Block (Block(..))
import App.Level (Level)
import App.Rect (Rect)

data Editor = Editor
  { level :: Level
  , selection :: Maybe Selection
  }
  deriving (Show, Generic)

data Selection
  = BlockSelection { blockId :: Int }
  | BoundsSelection
  | CollectorColumnSelection
  | TileSelection { pos :: V2 Int }
  deriving (Show, Generic)

fromLevel :: Level -> Editor
fromLevel level = Editor
  { level = level
  , selection = Nothing
  }

selectionRect :: Editor -> Maybe (Rect Int)
selectionRect Editor{ selection, level } =
  case selection of
    Just BlockSelection { blockId } -> level ^? #blockById . at blockId . _Just . #rect
    Just BoundsSelection -> Just $ level ^. #bounds
    Just CollectorColumnSelection -> Just $ Level.collectorRect level
    Just TileSelection{ pos } -> Just $ Rect.fromMinSize pos 1
    Nothing -> Nothing

selectTileAt :: V2 Float -> Editor -> Editor
selectTileAt (fmap floor -> pos) editor =
  let block = editor ^. #level . #blockById
        & toList
        & find (\Block{ rect } -> Rect.contains rect pos)
  in case block of
    Just Block{ uid } -> editor & #selection .~ Just (BlockSelection uid)
    Nothing -> editor & #selection .~ Just (TileSelection{ pos })

selectBounds :: Editor -> Editor
selectBounds editor =
  editor & #selection .~ Just BoundsSelection

selectCollectorColumn :: Editor -> Editor
selectCollectorColumn editor =
  editor & #selection .~ Just CollectorColumnSelection

moveSelection :: V2 Int -> Editor -> Editor
moveSelection dir editor@Editor{ selection } =
  case selection of
    Just BlockSelection{ blockId } ->
      editor & #level . #blockById . at blockId . _Just . #rect . #xy +~ dir
    Just BoundsSelection ->
      editor & #level . #bounds . #xy +~ dir
    Just CollectorColumnSelection ->
      editor & #level . #collectorColumn +~ dir ^. _x
    Just TileSelection{} -> editor
    Nothing -> editor

resizeSelection :: V2 Int -> Editor -> Editor
resizeSelection dir editor@Editor{ selection } =
  case selection of
    Just BlockSelection{ blockId } ->
      editor & #level . #blockById . at blockId . _Just . #rect . #wh %~ \wh ->
        max 1 <$> wh + dir
    Just BoundsSelection ->
      editor & #level . #bounds . #wh %~ \wh ->
        max 1 <$> wh + dir
    Just CollectorColumnSelection -> editor
    Just TileSelection{} -> editor
    Nothing -> editor

orientSelection :: V2 Int -> Editor -> Editor
orientSelection dir editor@Editor{ selection } =
  case selection of
    Just BlockSelection{ blockId } ->
      editor & #level . #blockById . at blockId . _Just . #orientation .~ dir
    Just BoundsSelection -> editor
    Just CollectorColumnSelection -> editor
    Just TileSelection{} -> editor
    Nothing -> editor

setSelectionBehavior :: Block.Behavior -> Editor -> Editor
setSelectionBehavior behavior editor@Editor{ selection } =
  case selection of
    Just BlockSelection{ blockId } ->
      editor & #level . #blockById . at blockId . _Just . #behavior .~ behavior
    Just BoundsSelection -> editor
    Just CollectorColumnSelection -> editor
    Just TileSelection{} -> editor
    Nothing -> editor

createBlock :: Editor -> Editor
createBlock editor@Editor{ selection, level } =
  case selection of
    Just TileSelection{ pos } ->
      let uid = case level ^. #blockById & IntMap.keysSet of
            Empty -> 0
            keys -> 1 + IntSet.findMax keys 
          block = Block
            { uid
            , rect = Rect.fromMinSize pos 1
            , orientation = V2 1 0
            , behavior = Block.Static
            }
      in editor
        & #level . #blockById . at uid .~ Just block
        & #selection .~ Just BlockSelection{ blockId = uid }
    Just BlockSelection{} -> editor
    Just BoundsSelection{} -> editor
    Just CollectorColumnSelection{} -> editor
    Nothing -> editor

deleteSelection :: Editor -> Editor
deleteSelection editor@Editor{ selection } =
  case selection of
    Just BlockSelection{ blockId } ->
      editor & #level . #blockById . at blockId .~ Nothing
    Just BoundsSelection{} -> editor
    Just CollectorColumnSelection -> editor
    Just TileSelection{} -> editor
    Nothing -> editor