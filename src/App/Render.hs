module App.Render
  ( render )
where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera
import qualified App.Editor as Editor
import qualified App.Rect as Rect
import qualified Linear as Lin
import qualified SDL as SDL

import App.Block (Block(..))
import App.Camera (Camera)
import App.GameState (GameState)
import App.Rect (Rect)
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
renderEditor renderer gs = 
  case view #_editor gs of
    Just editor -> do
      SDL.rendererDrawColor renderer $= V4 0 0 31 255
      SDL.clear renderer
      let blocks = editor
            & view (#_level . #_blockById)
            & toList
            & map (fmap fromIntegral)
          camera = fromIntegral <$> view #_camera gs
      SDL.rendererDrawColor renderer $= V4 191 191 191 255
      renderBlocks renderer camera blocks
      case editor ^. #_selection of
        Just Editor.BoundsSelection -> SDL.rendererDrawColor renderer $= V4 255 191 255 255
        _ -> SDL.rendererDrawColor renderer $= V4 191 191 191 255
      let levelBounds = fromIntegral <$> view (#_level . #_bounds) editor
      SDL.drawRect renderer . Just . drawnRect camera $ levelBounds
      SDL.rendererDrawColor renderer $= V4 191 191 191 255
      for_ (editor & Editor.selectedBlock) $ \Block{ _rect } ->
        SDL.drawRect renderer . Just . drawnRect camera $ fmap fromIntegral _rect
    Nothing -> pure ()

renderBlocks :: SDL.Renderer -> Camera Float -> [Block Float] -> IO ()
renderBlocks renderer camera blocks = 
  for_ blocks $ \block -> do
    let rect = view #_rect block
        scrRect = drawnRect camera rect
        dir = fromIntegral <$> block ^. #_orientation
        behavior = view #_behavior block
    SDL.rendererDrawColor renderer $= 
      case behavior of
        Block.Static -> V4 91 91 91 255
        _ -> V4 127 127 127 255
    SDL.fillRect renderer $ Just scrRect
    SDL.rendererDrawColor renderer $= V4 63 63 63 255
    SDL.drawRect renderer $ Just scrRect
    case behavior of
      Block.Movable -> do
        let scrMarkerRect = drawnRect camera $ sideMarkerRect rect dir
        SDL.rendererDrawColor renderer $= V4 0 255 0 255
        SDL.fillRect renderer $ Just scrMarkerRect
      Block.Flippable { Block._flipped = flipped } -> do
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
            & over (#_rect . #_xy) (\src -> Lin.lerp t src (fromIntegral <$> tgt))
          )
    Nothing -> []

drawnRect :: Camera Float -> Rect Float -> SDL.Rectangle CInt
drawnRect camera =
  Rect.toSdl . fmap round . Camera.rectToScreen camera

sideMarkerRect :: Rect Float -> V2 Float -> Rect Float
sideMarkerRect rect dir =
  Rect.fromCenterRadius 
    (Rect.center rect + (0.5 *^ view #_wh rect - 0.15) * dir) 
    0.08