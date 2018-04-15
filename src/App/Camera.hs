module App.Camera where

import App.Prelude

data Camera = Camera
  { _scale :: Int
  , _translate :: V2 Int
  }
  deriving (Show, Generic)

screenToPoint :: Camera -> V2 Int -> V2 Double
screenToPoint (Camera scale translate) p =
  (fmap fromIntegral p - fmap fromIntegral translate) / fromIntegral scale

pointToScreen :: Camera -> V2 Int -> V2 Int
pointToScreen (Camera scale translate) p =
  scale *^ p + translate

vectorToScreen :: Camera -> V2 Int -> V2 Int
vectorToScreen (Camera scale _) p =
  scale *^ p

initial :: Camera
initial = Camera
  { _translate = V2 256 256
  , _scale = 64
  }
