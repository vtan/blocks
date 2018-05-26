module App.Render
  ( render )
where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera
import qualified App.Editor as Editor
import qualified App.GameState as GameState
import qualified App.Level as Level
import qualified App.Rect as Rect
import qualified Linear as Lin
import qualified SDL as SDL

import App.Block (Block(..))
import App.Camera (Camera)
import App.Editor (Editor)
import App.GameState (GameState)
import App.Level (Level(..))
import App.Rect (Rect)
import SDL (($=))

render :: SDL.Renderer -> GameState -> IO ()
render renderer gs = do
  case gs ^. #editor of
    Just editor -> renderEditor renderer editor gs
    Nothing -> renderGame renderer gs
  SDL.present renderer

renderGame :: SDL.Renderer -> GameState -> IO ()
renderGame renderer gs = do
  SDL.rendererDrawColor renderer $= 
    if GameState.levelWon gs
    then V4 127 107 0 255
    else V4 0 0 0 255
  SDL.clear renderer
  let blocks = (staticBlocks <> animatedBlocks) gs
      camera = fromIntegral <$> view #camera gs
  gs ^. #currentLevel & renderLevelBg renderer camera
  renderBlocks renderer camera blocks
  SDL.rendererDrawColor renderer $= V4 191 191 191 255
  let levelBounds = view (#currentLevel . #bounds) gs
  SDL.drawRect renderer . Just . drawnRect' camera $ levelBounds

renderEditor :: SDL.Renderer -> Editor -> GameState -> IO ()
renderEditor renderer editor gs = do
  SDL.rendererDrawColor renderer $= V4 0 0 31 255
  SDL.clear renderer
  let blocks = editor
        & view (#level . #blockById)
        & toList
        & map (fmap fromIntegral)
      camera = fromIntegral <$> view #camera gs
  editor ^. #level & renderLevelBg renderer camera
  SDL.rendererDrawColor renderer $= V4 191 191 191 255
  renderBlocks renderer camera blocks
  SDL.rendererDrawColor renderer $= V4 191 191 191 255
  let levelBounds = view (#level . #bounds) editor
  SDL.drawRect renderer . Just . drawnRect' camera $ levelBounds
  for_ (editor & Editor.selectionRect) $ \rect -> do
    SDL.rendererDrawColor renderer $= V4 255 191 255 255
    SDL.drawRect renderer . Just . drawnRect' camera $ rect

renderBlocks :: SDL.Renderer -> Camera Float -> [Block Float] -> IO ()
renderBlocks renderer camera blocks = 
  for_ blocks $ \block -> do
    let rect = view #rect block
        scrRect = drawnRect camera rect
        dir = fromIntegral <$> block ^. #orientation
        behavior = view #behavior block
    SDL.rendererDrawColor renderer $= 
      case behavior of
        Block.Static -> V4 91 91 91 255
        _ -> V4 127 127 127 255
    SDL.fillRect renderer $ Just scrRect
    SDL.rendererDrawColor renderer $= V4 71 71 71 255
    SDL.drawRect renderer $ Just scrRect
    case behavior of
      Block.Movable -> do
        let scrMarkerRect = drawnRect camera $ sideMarkerRect rect dir
        SDL.rendererDrawColor renderer $= V4 0 255 0 255
        SDL.fillRect renderer $ Just scrMarkerRect
      Block.Flippable { Block.flipped } -> do
        let scrCurrentMarkerRect = drawnRect camera
              $ sideMarkerRect rect (dir & if flipped then negate else id)
            scrOtherMarkerRect = drawnRect camera
              $ sideMarkerRect rect (dir & if flipped then id else negate)
        SDL.rendererDrawColor renderer $= V4 0 255 0 255
        SDL.fillRect renderer $ Just scrCurrentMarkerRect
        SDL.rendererDrawColor renderer $= V4 255 0 0 255
        SDL.fillRect renderer $ Just scrOtherMarkerRect
      Block.Collectable -> do
        let centerRect = Rect.fromCenterRadius (Rect.center rect) 0.15
        SDL.rendererDrawColor renderer $= V4 0 255 255 255
        SDL.fillRect renderer . Just . drawnRect camera $ centerRect
      Block.Static -> pure ()
      Block.Pushable -> pure ()

renderLevelBg :: SDL.Renderer -> Camera Float -> Level -> IO ()
renderLevelBg renderer camera level@Level{ bounds } = do
  SDL.rendererDrawColor renderer $= V4 31 31 31 255
  SDL.fillRect renderer . Just . drawnRect' camera $ bounds
  SDL.rendererDrawColor renderer $= V4 31 47 31 255
  SDL.fillRect renderer . Just . drawnRect' camera $ Level.collectorRect level

staticBlocks :: GameState -> [Block Float]
staticBlocks gs =
  case view #currentAnimation gs of
    Just anim -> view #otherBlocksById anim
    Nothing -> toList $ view #blockById gs
  & map (fmap fromIntegral)

animatedBlocks :: GameState -> [Block Float]
animatedBlocks gs =
  case view #currentAnimation gs of
    Just anim ->
      let start = view #start anim
          end = view #end anim
          t = (end - view #totalTime gs) / (end - start)
      in view #movingBlocksById anim
        & toList
        & map (\(block, tgt) -> block
            & fmap fromIntegral
            & over (#rect . #xy) (\src -> Lin.lerp t src (fromIntegral <$> tgt))
          )
    Nothing -> []

drawnRect :: Camera Float -> Rect Float -> SDL.Rectangle CInt
drawnRect camera =
  Rect.toSdl . fmap round . Camera.rectToScreen camera

drawnRect' :: Integral a => Camera Float -> Rect a -> SDL.Rectangle CInt
drawnRect' camera = drawnRect camera . fmap fromIntegral

sideMarkerRect :: Rect Float -> V2 Float -> Rect Float
sideMarkerRect rect dir =
  Rect.fromCenterRadius 
    (Rect.center rect + (0.5 *^ view #wh rect - 0.15) * dir) 
    0.08