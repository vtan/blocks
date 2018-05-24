module App.Editor where

import App.Prelude

import qualified App.Block as Block
import qualified App.Rect as Rect

import App.Block (Block(..))
import App.Level (Level)

data Editor = Editor
  { level :: Level
  , selection :: Maybe Selection
  }
  deriving (Show, Generic)

data Selection
  = BlockSelection { blockId :: Int }
  | BoundsSelection
  deriving (Show, Generic)

fromLevel :: Level -> Editor
fromLevel level = Editor
  { level = level
  , selection = Nothing
  }

selectedBlock :: Editor -> Maybe (Block Int)
selectedBlock editor@Editor{ selection } =
  case selection of
    Just BlockSelection{ blockId } -> editor ^. #level . #blockById . at blockId
    _ -> Nothing

selectBlockAt :: V2 Float -> Editor -> Editor
selectBlockAt (fmap floor -> pos) editor =
  let block = editor ^. #level . #blockById
        & toList
        & find (\Block{ rect } -> Rect.contains rect pos)
  in case block of
    Just Block{ uid } -> editor & #selection .~ Just (BlockSelection uid)
    Nothing -> editor

selectBounds :: Editor -> Editor
selectBounds editor =
  editor & #selection .~ Just BoundsSelection

moveSelection :: V2 Int -> Editor -> Editor
moveSelection dir editor@Editor{ selection } =
  case selection of
    Just BlockSelection{ blockId } ->
      editor & #level . #blockById . at blockId . _Just . #rect . #xy +~ dir
    Just BoundsSelection ->
      editor & #level . #bounds . #xy +~ dir
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
    Nothing -> editor

orientSelection :: V2 Int -> Editor -> Editor
orientSelection dir editor@Editor{ selection } =
  case selection of
    Just BlockSelection{ blockId } ->
      editor & #level . #blockById . at blockId . _Just . #orientation .~ dir
    Just BoundsSelection -> editor
    Nothing -> editor

setSelectionBehavior :: Block.Behavior -> Editor -> Editor
setSelectionBehavior behavior editor@Editor{ selection } =
  case selection of
    Just BlockSelection{ blockId } ->
      editor & #level . #blockById . at blockId . _Just . #behavior .~ behavior
    Just BoundsSelection -> editor
    Nothing -> editor