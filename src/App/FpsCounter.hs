module App.FpsCounter (new, withFps) where

import App.Prelude

import qualified SDL.Raw

import Data.IORef
import Data.Word (Word64)

sampledFrameCount :: Int
sampledFrameCount = 20

data Counter = Counter
  { _frameCount :: IORef Int
  , _counterSum :: IORef Word64
  , _counterFrequency :: Word64
  }

new :: IO Counter
new = do
  freq <- SDL.Raw.getPerformanceFrequency
  count <- newIORef 0
  s <- newIORef 0
  pure $ Counter count s freq

withFps :: Counter -> (Maybe Float -> IO a) -> IO a
withFps (Counter countRef sumRef freq) f = do
  start <- SDL.Raw.getPerformanceCounter
  count <- readIORef countRef
  s <- readIORef sumRef
  let updateNow = (count == sampledFrameCount)
      fps
        | updateNow = Just $ 1 / (fromIntegral s / fromIntegral count / fromIntegral freq)
        | otherwise = Nothing
  x <- f fps
  end <- SDL.Raw.getPerformanceCounter
  writeIORef countRef (if updateNow then 0 else count + 1)
  writeIORef sumRef (if updateNow then 0 else s + (end - start))
  pure x
