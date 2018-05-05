module App.Render
  ( render )
where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera
import qualified App.Editor as Editor
import qualified App.Rect as Rect
import qualified SDL as SDL

import App.Block (Block)
import App.Camera (Camera)
import App.GameState (GameState)
import App.Rect (Rect)
import Linear (lerp)
import SDL (($=))

render :: SDL.Renderer -> GameState -> IO ()
render renderer gs = do
  if has (#_editor . _Just) gs
  then renderEditor renderer gs
  else renderGame renderer gs
  SDL.present renderer

renderGame :: SDL.Renderer -> GameState -> IO ()
renderGame renderer gs = do
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  let blocks = (staticBlocks <> animatedBlocks) gs
      camera = fromIntegral <$> view #_camera gs
  renderBlocks renderer camera blocks
  SDL.rendererDrawColor renderer $= V4 191 191 191 255
  let levelBounds = fromIntegral <$> view (#_currentLevel . #_bounds) gs
  SDL.drawRect renderer . Just . drawnRect camera $ levelBounds

renderEditor :: SDL.Renderer -> GameState -> IO ()
renderEditor renderer gs = do
  SDL.rendererDrawColor renderer $= V4 0 0 31 255
  SDL.clear renderer
  SDL.P mousePos <- SDL.getAbsoluteMouseLocation
  let mouseTile = Camera.screenToPoint (view #_camera gs) $ fmap fromIntegral mousePos
      -- TODO move this to App.Editor?
      editedBlocks = case preview (#_editor . _Just . #_currentAction . _Just) gs of
        Just (Editor.MoveBlock block grabbedPoint) -> 
          [fmap fromIntegral block & over (#_rect . #_xy) (+ (mouseTile - grabbedPoint))]
        Just (Editor.ResizeBlock block grabbedPoint moveOrigin) -> 
          [fmap fromIntegral block & over #_rect (Editor.resize moveOrigin (mouseTile - grabbedPoint))]
        Nothing -> []
      snapRects = editedBlocks & map (
          view #_rect
          >>> over #_xy (fmap (fromIntegral @Int @Float . round))
          >>> over #_wh (fmap (fromIntegral @Int . round))
        )
      staticBlocks = gs
        & view (#_editor . _Just . #_level . #_blockById)
        & toList
        & map (fmap fromIntegral)
        & filter (\b -> not $ any (Block.eqId b) editedBlocks)
      blocks = staticBlocks <> editedBlocks
      camera = fromIntegral <$> view #_camera gs
  SDL.rendererDrawColor renderer $= V4 191 191 191 255
  for_ snapRects $ \r -> SDL.drawRect renderer (Just $ drawnRect camera r)
  renderBlocks renderer camera blocks
  SDL.rendererDrawColor renderer $= V4 191 191 191 255
  let levelBounds = fromIntegral <$> view (#_currentLevel . #_bounds) gs
  SDL.drawRect renderer . Just . drawnRect camera $ levelBounds

renderBlocks :: SDL.Renderer -> Camera Float -> [Block Float] -> IO ()
renderBlocks renderer camera blocks = 
  for_ blocks $ \block -> do
    let rect = view #_rect block
        scrRect = drawnRect camera rect
    SDL.rendererDrawColor renderer $= V4 127 127 127 255
    SDL.fillRect renderer $ Just scrRect
    SDL.rendererDrawColor renderer $= V4 63 63 63 255
    SDL.drawRect renderer $ Just scrRect
    case view #_behavior block of
      Block.Movable { Block._direction = (fmap fromIntegral -> dir) } -> do
        let scrMarkerRect = drawnRect camera $ sideMarkerRect rect dir
        SDL.rendererDrawColor renderer $= V4 0 255 0 255
        SDL.fillRect renderer $ Just scrMarkerRect
      Block.Flippable { Block._direction = (fmap fromIntegral -> dir), Block._flipped = flipped } -> do
        let scrCurrentMarkerRect = drawnRect camera
              $ sideMarkerRect rect (dir & if flipped then negate else id)
            scrOtherMarkerRect = drawnRect camera
              $ sideMarkerRect rect (dir & if flipped then id else negate)
        SDL.rendererDrawColor renderer $= V4 0 255 0 255
        SDL.fillRect renderer $ Just scrCurrentMarkerRect
        SDL.rendererDrawColor renderer $= V4 255 0 0 255
        SDL.fillRect renderer $ Just scrOtherMarkerRect
      Block.Static -> pure ()
      Block.Pushable -> pure ()

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

drawnRect :: Camera Float -> Rect Float -> SDL.Rectangle CInt
drawnRect camera =
  Rect.toSdl . fmap round . Camera.rectToScreen camera

sideMarkerRect :: Rect Float -> V2 Float -> Rect Float
sideMarkerRect rect dir =
  Rect.fromCenterRadius (Rect.center rect + 0.35 *^ dir) 0.08