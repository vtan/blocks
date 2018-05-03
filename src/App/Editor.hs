module App.Editor where

import App.Prelude

import App.Level (Level)

data Editor = Editor
  { _level :: Level
  , _currentAction :: Maybe Action
  }
  deriving (Show, Generic)

data Action
  = MoveBlock { _id :: Int, _from :: V2 Int }
  deriving (Show, Generic)

fromLevel :: Level -> Editor
fromLevel level = Editor
  { _level = level
  , _currentAction = Nothing
  }