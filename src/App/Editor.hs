module App.Editor where

import App.Prelude

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
  deriving (Show, Generic)

fromLevel :: Level -> Editor
fromLevel level = Editor
  { _level = level
  , _currentAction = Nothing
  }