module App.Render where

import App.Prelude

import qualified App.Camera as Camera
import qualified Data.IntMap.Strict as IntMap
import qualified SDL as SDL

import App.Block (Block)
import App.Camera (Camera)
import App.GameState (GameState)
import Linear (lerp)
import SDL (($=))

render :: SDL.Renderer -> GameState -> IO ()
render renderer gs = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  let rects = (calculateStaticRects <> calculateAnimatedRects) gs
  for_ rects $ \rect -> do
    SDL.rendererDrawColor renderer $= V4 127 127 127 255
    SDL.fillRect renderer $ Just rect
    SDL.rendererDrawColor renderer $= V4 63 63 63 255
    SDL.drawRect renderer $ Just rect
  SDL.present renderer

calculateStaticRects :: Integral a => GameState -> [SDL.Rectangle a]
calculateStaticRects gs =
  map (fmap fromIntegral . calculateBlockRect camera) blocks
  where
    camera = view #_camera gs
    blocks = toList $ IntMap.difference
      (view #_blockById gs)
      (view (#_currentAnimation . _Just . #_blockTargetsById) gs)

calculateAnimatedRects :: Integral a => GameState -> [SDL.Rectangle a]
calculateAnimatedRects gs =
  case view #_currentAnimation gs of
    Just anim ->
      let start = view #_start anim
          end = view #_end anim
          targets = view #_blockTargetsById anim
          t = (end - view #_totalTime gs) / (end - start)
      in IntMap.intersectionWith (,) (view #_blockById gs) targets
        & toList
        & map (\(block, tgt) ->
            -- TODO use fmap or <$> consistently
            let src = view #_origin block
                origin = round <$> Camera.pointToScreen camera'
                  (lerp t (fromIntegral <$> src) (fromIntegral <$> tgt))
                extent = Camera.vectorToScreen camera $ view #_extent block
            in fromIntegral <$> SDL.Rectangle (SDL.P origin) extent
          )
    Nothing -> []
  where
    camera = view #_camera gs
    camera' = fmap fromIntegral camera

-- TODO abstract rect?
calculateBlockRect :: Num a => Camera a -> Block -> SDL.Rectangle a
calculateBlockRect camera block =
  SDL.Rectangle (SDL.P origin) extent
  where
    origin = Camera.pointToScreen camera . fmap fromIntegral $ view #_origin block
    extent = Camera.vectorToScreen camera . fmap fromIntegral $ view #_extent block
