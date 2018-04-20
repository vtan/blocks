module App.Camera where

import App.Prelude

data Camera a = Camera
  { _scale :: a
  , _translate :: V2 a
  }
  deriving (Show, Generic, Functor)

screenToPoint :: (Integral a, Fractional b) => Camera a -> V2 a -> V2 b
screenToPoint (Camera scale translate) p =
  (fmap fromIntegral p - fmap fromIntegral translate) / fromIntegral scale

pointToScreen :: Num a => Camera a -> V2 a -> V2 a
pointToScreen (Camera scale translate) p =
  scale *^ p + translate

vectorToScreen :: Num a => Camera a -> V2 a -> V2 a
vectorToScreen (Camera scale _) p =
  scale *^ p

initial :: Camera Int
initial = Camera
  { _translate = V2 64 64
  , _scale = 48
  }
