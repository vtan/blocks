module App.Update where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera
import qualified App.GameState as GameState
import qualified Control.Monad.Writer.CPS as Writer
import qualified Data.IntMap.Strict as IntMap
import qualified SDL as SDL

import App.Block (Block)
import App.GameState (GameState)

update :: Float -> [SDL.Event] -> GameState -> GameState
update lastFrameTime events =
  over #_totalTime (+ lastFrameTime)
  >>> \gs ->
  -- TODO skip only mouse handling when animating
    let cont = \gs' -> foldl' handleEvent gs' events
     in case view #_currentAnimation gs of
          Just anim ->
            if view #_end anim < view #_totalTime gs
            then cont (view #_after anim & set #_currentAnimation Nothing)
            else gs
          Nothing -> cont gs

handleEvent :: GameState -> SDL.Event -> GameState
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

blockClicked :: Block -> GameState -> GameState
blockClicked block gs =
  case view #_behavior block of
    Block.Static -> gs
    Block.Movable dir ->
      case Writer.runWriterT (tryPush block dir gs) of
        Just (gs', targets) ->
          let time = view #_totalTime gs
              anim = GameState.Animation
                { GameState._start = time
                -- TODO magic number
                , GameState._end = time + 1
                , GameState._blockTargetsById = targets
                , GameState._after = gs'
                }
           in gs & set #_currentAnimation (Just anim)
        Nothing -> gs
    Block.Pushable -> gs

tryPush :: Block -> V2 Int -> GameState -> WriterT (IntMap (V2 Int)) Maybe GameState
tryPush block dir gs =
  case view #_behavior block of
    Block.Static -> empty
    Block.Movable _ -> gs'
    Block.Pushable -> gs'
  where
    gs' = allBlocks
      & toList
      & filter (not . Block.eqId block)
      & filter (Block.intersects movedBlock)
      & foldlM (\gs'' b -> tryPush b dir gs'') gs
      & fmap (set (#_blockById . at blockId . _Just) movedBlock)
      & collect (IntMap.singleton blockId $ view #_origin movedBlock)
    allBlocks = gs & view #_blockById
    blockId = block & view #_id
    movedBlock = block & over #_origin (+ dir)
    collect w x = x <* Writer.tell w
