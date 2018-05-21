module App.Rect where

import App.Prelude

import qualified Linear.Affine as Lin
import qualified SDL

data Rect a = Rect
  { _xy :: V2 a
  , _wh :: V2 a
  }
  deriving (Show, Generic, Functor)

fromCenterRadius :: Num a => V2 a -> V2 a -> Rect a
fromCenterRadius c r = Rect (c - r) (2 *^ r)

center :: Fractional a => Rect a -> V2 a
center (Rect xy wh) = xy + wh / 2

contains :: (Num a, Ord a) => Rect a -> V2 a -> Bool
contains (Rect (V2 x y) (V2 w h)) (V2 px py) =
  px >= x && px < x + w && py >= y && py < y + h

containsRect :: (Num a, Ord a) => Rect a -> Rect a -> Bool
containsRect (Rect (V2 x1 y1) (V2 w1 h1)) (Rect (V2 x2 y2) (V2 w2 h2)) =
  x2 >= x1 && x2 + w2 <= x1 + w1 && y2 >= y1 && y2 + h2 <= y1 + h1

intersects :: (Num a, Ord a) => Rect a -> Rect a -> Bool
intersects (Rect (V2 ax ay) (V2 aw ah)) (Rect (V2 bx by) (V2 bw bh)) = 
  abs (ax - bx) * 2 < aw + bw && abs (ay - by) * 2 < ah + bh

toSdl :: Rect a -> SDL.Rectangle a
toSdl (Rect xy wh) = SDL.Rectangle (Lin.P xy) wh