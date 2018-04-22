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

animationLength :: Float
animationLength = 0.2

update :: Float -> [SDL.Event] -> GameState -> GameState
update lastFrameTime events =
  over #_totalTime (+ lastFrameTime)
  >>> updateAnimation
  >>> (\gs -> foldl' handleEvent gs events)

updateAnimation :: GameState -> GameState
updateAnimation gs =
  view #_currentAnimation gs & maybe gs (\anim ->
    if view #_end anim < view #_totalTime gs
    then view #_after anim
    else gs
  )

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
    } | gs & has (#_currentAnimation . _Nothing) ->
    let clickTile = floor @Double <$>
          Camera.screenToPoint (view #_camera gs) (fmap fromIntegral clickPos)
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
        Just (gs', moves) ->
          let anim = animateMoves gs gs' moves
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

animateMoves :: GameState -> GameState -> IntMap (V2 Int) -> GameState.Animation
animateMoves gs gs' moves =
  GameState.Animation
    { GameState._start = time
    , GameState._end = time + animationLength
    , GameState._movingBlocksById = movingBlocks
    , GameState._otherBlocksById = otherBlocks
    , GameState._after = gs'
    }
  where
    blocks = view #_blockById gs
    time = view #_totalTime gs
    movingBlocks = toList $ IntMap.intersectionWith (,) blocks moves
    otherBlocks = toList $ IntMap.difference blocks moves