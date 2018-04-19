module App.FpsCounter (Counter, new, record) where

import App.Prelude

import qualified SDL.Raw

import Data.Word (Word64)

sampledFrameCount :: Int
sampledFrameCount = 20

data Counter = Counter
  { _counterFrequency :: Word64
  , _frameCount :: Int
  , _counterSum :: Word64
  , _updatedFps :: Maybe Float
  , _lastFrameTime :: Float
  }
  deriving (Show, Generic)

new :: IO Counter
new = do
  freq <- SDL.Raw.getPerformanceFrequency
  pure $ Counter
    { _counterFrequency = freq
    , _frameCount = 0
    , _counterSum = 0
    , _updatedFps = Nothing
    , _lastFrameTime = 0
    }

record :: Counter -> Word64 -> Counter
record counter lastFrame =
  counter
    & over #_frameCount (+ 1)
    & over #_counterSum (+ lastFrame)
    & set #_updatedFps Nothing
    -- TODO: Might as well store the sum in seconds too
    & set #_lastFrameTime (fromIntegral lastFrame / fromIntegral (view #_counterFrequency counter))
    & updateIfNeeded

updateIfNeeded :: Counter -> Counter
updateIfNeeded counter
  | count == sampledFrameCount =
      counter
        & set #_frameCount 0
        & set #_counterSum 0
        & set #_updatedFps (Just $ 1 / (fromIntegral s / fromIntegral count / fromIntegral freq))
  | otherwise = counter
  where
    Counter{ _counterFrequency = freq, _frameCount = count, _counterSum = s } = counter
