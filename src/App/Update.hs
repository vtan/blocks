module App.Update where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera
import qualified App.GameState as GameState
import qualified SDL as SDL

update :: [SDL.Event] -> GameState.GameState -> GameState.GameState
update events gs = foldl' handleEvent gs events

handleEvent :: GameState.GameState -> SDL.Event -> GameState.GameState
handleEvent gs = \case
  SDL.Event { SDL.eventPayload = SDL.QuitEvent } ->
    set #_quit True gs
  SDL.Event
    { SDL.eventPayload = SDL.MouseButtonEvent
      ( SDL.MouseButtonEventData
        { SDL.mouseButtonEventMotion = SDL.Released
        , SDL.mouseButtonEventButton = SDL.ButtonLeft
        , SDL.mouseButtonEventPos = SDL.P (clickPos)
        }
      )
    } ->
    let clickTile = floor <$> Camera.screenToPoint (view #_camera gs) (fmap fromIntegral clickPos)
        clickBlock = do
          clickedBlockId <- view (#_blockIdByOrigin . at clickTile) gs
          view (#_blockById . at clickedBlockId) gs
     in maybe gs (flip blockClicked gs) clickBlock
  _ ->
    gs

blockClicked :: Block.Block -> GameState.GameState -> GameState.GameState
blockClicked block gs =
  case view #_behavior block of
    Block.Static -> gs
    Block.Movable dir ->
      let blockId = view #_id block
          origin = view #_origin block
          origin' = origin + dir
       in gs
            & set (#_blockById . at blockId . _Just . #_origin) origin'
            & set (#_blockIdByOrigin . at origin) Nothing
            & set (#_blockIdByOrigin . at origin') (Just blockId)
