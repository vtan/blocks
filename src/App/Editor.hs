module App.Editor where

import App.Prelude

import qualified App.Rect as Rect

import App.Block (Block(..))
import App.Level (Level)

data Editor = Editor
  { _level :: Level
  , _selection :: Maybe Selection
  , _currentAction :: Maybe Action
  }
  deriving (Show, Generic)

data Selection
  = BlockSelection { _blockId :: Int }
  | BoundsSelection
  deriving (Show, Generic)

data Action
  = MoveBlock 
    { _block :: Block Int 
    , _grabbedPoint :: V2 Float
    }
  | ResizeBlock 
    { _block :: Block Int 
    , _grabbedPoint :: V2 Float
    , _resizeDir :: V2 Bool
    }
  | OverBlockCorner
    { _block :: Block Int
    , _corner :: Rect.Corner
    , _cornerPos :: V2 Float
    }
  deriving (Show, Generic)

fromLevel :: Level -> Editor
fromLevel level = Editor
  { _level = level
  , _selection = Nothing
  , _currentAction = Nothing
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

editBlock :: V2 Float -> Editor -> Maybe (Block Float)
editBlock mousePos editor =
  view #_currentAction editor >>= \case
    MoveBlock (fmap fromIntegral -> block) grabbedPoint ->
      let delta = mousePos - grabbedPoint
      in Just (block & over (#_rect . #_xy) (+ delta))
    ResizeBlock (fmap fromIntegral -> block) grabbedPoint reverseDir ->
      let delta = mousePos - grabbedPoint
      in Just (block & over #_rect (Rect.extendCorner reverseDir delta))
    OverBlockCorner{} -> Nothing

endEdit :: V2 Float -> Editor -> Maybe Editor
endEdit mousePos editor =
  editBlock mousePos editor <&> \block' ->
    editor
      & set (#_level . #_blockById . at (view #_id block') . _Just) (round <$> block')
      & set #_currentAction Nothing