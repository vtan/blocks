module App.Render where

import App.Prelude

import qualified App.Camera as Camera
import qualified SDL as SDL

import App.GameState (GameState)
import SDL (($=))

render :: SDL.Renderer -> GameState -> IO ()
render renderer gs = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  let camera = view #_camera gs
  for_ (view #_blockById gs) $ \block -> do
    let origin = Camera.pointToScreen camera $ view #_origin block
        extent = Camera.vectorToScreen camera $ view #_extent block
        rect = fromIntegral <$> SDL.Rectangle (SDL.P origin) extent
    SDL.rendererDrawColor renderer $= V4 127 127 127 255
    SDL.fillRect renderer $ Just rect
    SDL.rendererDrawColor renderer $= V4 63 63 63 255
    SDL.drawRect renderer $ Just rect
  SDL.present renderer
