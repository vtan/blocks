module Main where

import App.Prelude

import qualified App.FpsCounter as FpsCounter
import qualified App.GameState as GameState
import qualified App.Render as Render
import qualified App.Update as Update
import qualified Data.Text as Text
import qualified SDL

import SDL (($=))
import Text.Printf (printf)

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) $
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  fpsCounter <- FpsCounter.new
  flip fix GameState.initial $ \cont gs -> do
    gs' <- FpsCounter.withFps fpsCounter $ \fps -> do
      case fps of
        Just updated -> SDL.windowTitle window $= Text.pack (printf "FPS: %.2f" updated)
        Nothing -> pure ()

      events <- SDL.pollEvents
      let !gs' = Update.update events gs
      Render.render renderer gs'
      pure gs'
    if (view #_quit gs') then pure () else cont gs'
  SDL.quit
