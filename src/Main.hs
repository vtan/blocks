module Main where

import qualified App.FpsCounter as FpsCounter
import qualified Data.Text as Text
import qualified SDL

import Data.Function (fix)
import SDL (($=))
import Text.Printf (printf)

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) $
    SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
  fpsCounter <- FpsCounter.new
  fix $ \cont -> do
    next <- FpsCounter.withFps fpsCounter $ \fps -> do
      case fps of
        Just updated -> SDL.windowTitle window $= Text.pack (printf "FPS: %.2f" updated)
        Nothing -> pure ()

      events <- SDL.pollEvents
      SDL.present renderer
      pure $ null [() | SDL.Event { SDL.eventPayload = SDL.QuitEvent } <- events]
    if next then cont else pure ()
  SDL.quit
