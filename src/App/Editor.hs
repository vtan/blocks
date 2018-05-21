module App.Editor where

import App.Prelude

import qualified App.Rect as Rect

import App.Block (Block(..))
import App.Level (Level)

data Editor = Editor
  { _level :: Level
  , _selection :: Maybe Selection
  }
  deriving (Show, Generic)

data Selection
  = BlockSelection { _blockId :: Int }
  | BoundsSelection
  deriving (Show, Generic)

fromLevel :: Level -> Editor
fromLevel level = Editor
  { _level = level
  , _selection = Nothing
  }

selectedBlock :: Editor -> Maybe (Block Int)
selectedBlock editor@Editor{ _selection } =
  case _selection of
    Just BlockSelection{ _blockId } -> editor ^. #_level . #_blockById . at _blockId
    _ -> Nothing

selectBlockAt :: V2 Float -> Editor -> Editor
selectBlockAt (fmap floor -> pos) editor =
  let block = editor ^. #_level . #_blockById
        & toList
        & find (\Block{ _rect } -> Rect.contains _rect pos)
  in case block of
    Just Block{ _id } -> editor & #_selection .~ Just (BlockSelection _id)
    Nothing -> editor

selectBounds :: Editor -> Editor
selectBounds editor =
  editor & #_selection .~ Just BoundsSelection

moveSelection :: V2 Int -> Editor -> Editor
moveSelection dir editor@Editor{ _selection } =
  case _selection of
    Just BlockSelection{ _blockId } ->
      editor & #_level . #_blockById . at _blockId . _Just . #_rect . #_xy +~ dir
    Just BoundsSelection ->
      editor & #_level . #_bounds . #_xy +~ dir
    Nothing -> editor

resizeSelection :: V2 Int -> Editor -> Editor
resizeSelection dir editor@Editor{ _selection } =
  case _selection of
    Just BlockSelection{ _blockId } ->
      editor & #_level . #_blockById . at _blockId . _Just . #_rect . #_wh %~ \wh ->
        max 1 <$> wh + dir
    Just BoundsSelection ->
      editor & #_level . #_bounds . #_wh %~ \wh ->
        max 1 <$> wh + dir
    Nothing -> editor