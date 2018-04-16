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
        clickBlock = GameState.findBlockAt gs clickTile
     in maybe gs (flip blockClicked gs) clickBlock
  _ ->
    gs

blockClicked :: Block.Block -> GameState.GameState -> GameState.GameState
blockClicked block gs =
  case view #_behavior block of
    Block.Static -> gs
    Block.Movable dir -> fromMaybe gs $ tryPush block dir gs
    Block.Pushable -> gs

tryPush :: Block.Block -> V2 Int -> GameState.GameState -> Maybe GameState.GameState
tryPush block dir gs =
  case view #_behavior block of
    Block.Static -> Nothing
    Block.Movable _ -> pushedGs
    Block.Pushable -> pushedGs
  where
    blockId = block & view #_id
    newPos = (block & view #_origin) + dir
    moveBlock = set (#_blockById . at blockId . _Just . #_origin) newPos
    pushedGs = case GameState.findBlockAt gs newPos of
      Just blockInWay ->
        tryPush blockInWay dir gs <&> moveBlock
      Nothing ->
        Just $ moveBlock gs
