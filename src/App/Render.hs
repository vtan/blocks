module App.Render
  ( render )
where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.Rect as Rect
import qualified SDL as SDL

import App.Block (Block)
import App.GameState (GameState)
import Linear (lerp)
import SDL (($=))

render :: SDL.Renderer -> GameState -> IO ()
render renderer gs = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  let blocks = (staticBlocks <> animatedBlocks) gs
      camera = view #_camera gs
      camera' = fromIntegral <$> camera
  for_ blocks $ \block -> do
    let rect = Rect.toSdl @CInt . fmap round . Camera.rectToScreen camera' $ view #_rect block
    SDL.rendererDrawColor renderer $= V4 127 127 127 255
    SDL.fillRect renderer $ Just rect
    SDL.rendererDrawColor renderer $= V4 63 63 63 255
    SDL.drawRect renderer $ Just rect
  SDL.rendererDrawColor renderer $= V4 191 191 191 255
  let levelBounds = view #_levelBounds gs
  SDL.drawRect renderer . Just . Rect.toSdl . Camera.rectToScreen camera $ levelBounds
  SDL.present renderer

staticBlocks :: GameState -> [Block Float]
staticBlocks gs =
  case view #_currentAnimation gs of
    Just anim -> view #_otherBlocksById anim
    Nothing -> toList $ view #_blockById gs
  & map (fmap fromIntegral)

animatedBlocks :: GameState -> [Block Float]
animatedBlocks gs =
  case view #_currentAnimation gs of
    Just anim ->
      let start = view #_start anim
          end = view #_end anim
          t = (end - view #_totalTime gs) / (end - start)
      in view #_movingBlocksById anim
        & toList
        & map (\(block, tgt) -> block
            & fmap fromIntegral
            & over (#_rect . #_xy) (\src -> lerp t src (fromIntegral <$> tgt))
          )
    Nothing -> []