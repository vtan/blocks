module App.Level where

import App.Prelude

import qualified App.Block as Block

import App.Block (Block(..))
import App.Rect (Rect(..))

data Level = Level
  { blockById :: IntMap (Block Int) -- TODO store as a Vector or List instead?
  , bounds :: Rect Int
  , collectorColumn :: Int
  }
  deriving (Show, Generic)

initial :: Level
initial = Level
  { blockById =
    [ (0, Block 0 (Rect (V2 4 0) (V2 1 1)) (V2 1 0) (Block.Flippable False))
    , (1, Block 1 (Rect (V2 6 1) (V2 1 1)) (V2 1 0) Block.Pushable)
    , (2, Block 2 (Rect (V2 5 0) (V2 1 2)) (V2 1 0) Block.Pushable)
    , (3, Block 3 (Rect (V2 7 0) (V2 1 1)) (V2 1 0) Block.Static)
    , (4, Block 4 (Rect (V2 4 2) (V2 1 1)) (V2 1 0) Block.Movable)
    ]
  , bounds = Rect (V2 3 (-1)) (V2 6 4)
  , collectorColumn = 4
  }

collectorRect :: Level -> Rect Int
collectorRect Level{ bounds, collectorColumn } =
  bounds
    & #xy . _x .~ collectorColumn
    & #wh . _x .~ 1