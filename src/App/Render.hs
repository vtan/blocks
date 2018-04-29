module App.Render
  ( render )
where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Rect as Rect
import qualified SDL as SDL

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
  SDL.rendererDrawColor renderer $= V4 191 191 191 255
  let camera = view #_camera gs
      levelBounds = view #_levelBounds gs
  SDL.drawRect renderer . Just . Rect.toSdl . Camera.rectToScreen camera $ levelBounds
  SDL.present renderer

calculateStaticRects :: GameState -> [SDL.Rectangle CInt]
calculateStaticRects gs =
  map (Rect.toSdl . Camera.rectToScreen camera . view #_rect) blocks
  where
    camera = view #_camera gs
    blocks = view #_currentAnimation gs
      & maybe (toList $ view #_blockById gs) (view #_otherBlocksById)

calculateAnimatedRects :: GameState -> [SDL.Rectangle CInt]
calculateAnimatedRects gs =
  case view #_currentAnimation gs of
    Just anim ->
      let start = view #_start anim
          end = view #_end anim
          t = (end - view #_totalTime gs) / (end - start)
      in view #_movingBlocksById anim
        & toList
        & map (\(block, tgt) ->
            let camera = fromIntegral <$> view #_camera gs
            in view #_rect block
              & fmap fromIntegral
              & over #_xy (\src -> lerp t src (fromIntegral <$> tgt))
              & Camera.rectToScreen camera
              & fmap round
              & Rect.toSdl @CInt
          )
    Nothing -> []