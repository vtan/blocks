module App.Rect where

import App.Prelude

data Rect a = Rect
  { _xy :: V2 a
  , _wh :: V2 a
  }
  deriving (Show, Generic, Functor)

contains :: (Num a, Ord a) => Rect a -> V2 a -> Bool
contains (Rect (V2 x y) (V2 w h)) (V2 px py) =
  px >= x && px < x + w && py >= y && py < y + h

intersects :: (Num a, Ord a) => Rect a -> Rect a -> Bool
intersects (Rect (V2 ax ay) (V2 aw ah)) (Rect (V2 bx by) (V2 bw bh)) = 
  abs (ax - bx) * 2 < aw + bw && abs (ay - by) * 2 < ah + bh