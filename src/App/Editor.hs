module App.Editor where

import App.Prelude

import qualified App.Rect as Rect

import App.Block (Block)
import App.Level (Level)

data Editor = Editor
  { _level :: Level
  , _currentAction :: Maybe Action
  }
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
  , _currentAction = Nothing
  }

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