module App.Render where

import App.Prelude

import qualified App.Camera as Camera
import qualified App.GameState as GameState
import qualified SDL as SDL

import SDL (($=))

render :: SDL.Renderer -> GameState.GameState -> IO ()
render renderer gs = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= V4 127 127 127 255
  let camera = view #_camera gs
  for_ (view #_blockById gs) $ \block ->
    let origin = Camera.pointToScreen camera $ view #_origin block
        extent = Camera.vectorToScreen camera $ view #_extent block
        rect = fromIntegral <$> SDL.Rectangle (SDL.P origin) extent
     in SDL.fillRect renderer $ Just rect
  SDL.present renderer
