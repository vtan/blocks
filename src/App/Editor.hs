module App.Editor where

import App.Prelude

import App.Block (Block)
import App.Level (Level)
import App.Rect (Rect(..))

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
    , _moveOrigin :: V2 Bool
    }
  deriving (Show, Generic)

fromLevel :: Level -> Editor
fromLevel level = Editor
  { _level = level
  , _currentAction = Nothing
  }

-- TODO cleanup
-- TODO make sure wh > 0
resize :: Num a => V2 Bool -> V2 a -> Rect a -> Rect a
resize moveOrigin delta (Rect xy wh) = Rect xy' wh'
  where
    xy' = (\b d coord -> if b then coord + d else coord) <$> moveOrigin <*> delta <*> xy
    wh' = (\b d coord -> if b then coord - d else coord + d) <$> moveOrigin <*> delta <*> wh