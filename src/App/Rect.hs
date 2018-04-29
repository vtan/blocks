module App.Rect where

import App.Prelude

import qualified SDL

import Linear.Affine (Point(P))

data Rect a = Rect
  { _xy :: V2 a
  , _wh :: V2 a
  }
  deriving (Show, Generic, Functor)

contains :: (Num a, Ord a) => Rect a -> V2 a -> Bool
contains (Rect (V2 x y) (V2 w h)) (V2 px py) =
  px >= x && px < x + w && py >= y && py < y + h

containsRect :: (Num a, Ord a) => Rect a -> Rect a -> Bool
containsRect (Rect (V2 x1 y1) (V2 w1 h1)) (Rect (V2 x2 y2) (V2 w2 h2)) =
  x2 >= x1 && x2 + w2 <= x1 + w1 && y2 >= y1 && y2 + h2 <= y1 + h1

intersects :: (Num a, Ord a) => Rect a -> Rect a -> Bool
intersects (Rect (V2 ax ay) (V2 aw ah)) (Rect (V2 bx by) (V2 bw bh)) = 
  abs (ax - bx) * 2 < aw + bw && abs (ay - by) * 2 < ah + bh

toSdl :: Integral a => Rect a -> SDL.Rectangle CInt
toSdl (Rect xy wh) = 
  fromIntegral <$> SDL.Rectangle (P xy) wh