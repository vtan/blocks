module App.Update 
  ( update )
where

import App.Prelude

import qualified App.Block as Block
import qualified App.Camera as Camera
import qualified App.Editor as Editor
import qualified App.GameState as GameState
import qualified App.Rect as Rect
import qualified Control.Monad.Writer.CPS as Writer
import qualified Data.IntMap.Strict as IntMap
import qualified Linear as Lin
import qualified SDL as SDL

import App.Block (Block)
import App.GameState (GameState)

pattern QuitEvent :: SDL.Event
pattern QuitEvent <-
  SDL.Event { SDL.eventPayload = SDL.QuitEvent }

pattern KeyPressEvent :: SDL.Scancode -> SDL.Event
pattern KeyPressEvent scancode <-
  SDL.Event 
    { SDL.eventPayload = SDL.KeyboardEvent 
      ( SDL.KeyboardEventData
        { SDL.keyboardEventKeyMotion = SDL.Pressed
        , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymScancode = scancode }
        }
      )
    }

pattern MouseReleaseEvent :: Num a => V2 a -> SDL.Event
pattern MouseReleaseEvent pos <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseButtonEvent
      ( SDL.MouseButtonEventData
        { SDL.mouseButtonEventMotion = SDL.Released
        , SDL.mouseButtonEventButton = SDL.ButtonLeft
        , SDL.mouseButtonEventPos = SDL.P (fmap fromIntegral -> pos)
        }
      )
    }

pattern MouseButtonEvent :: Num a => SDL.InputMotion -> V2 a -> SDL.Event
pattern MouseButtonEvent motion pos <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseButtonEvent
      ( SDL.MouseButtonEventData
        { SDL.mouseButtonEventMotion = motion
        , SDL.mouseButtonEventButton = SDL.ButtonLeft
        , SDL.mouseButtonEventPos = SDL.P (fmap fromIntegral -> pos)
        }
      )
    }

pattern MouseMotionEvent :: Num a => V2 a -> SDL.Event
pattern MouseMotionEvent pos <-
  SDL.Event
    { SDL.eventPayload = SDL.MouseMotionEvent
      ( SDL.MouseMotionEventData
        { SDL.mouseMotionEventPos = SDL.P (fmap fromIntegral -> pos) }
      )
    }

animationLength :: Float
animationLength = 0.2

update :: Float -> [SDL.Event] -> GameState -> GameState
update lastFrameTime events =
  over #_totalTime (+ lastFrameTime)
  >>> (\gs -> foldl' handleEvent gs events)
  >>> (\gs  -> if has (#_editor . _Nothing) gs
        then updateGame gs
        else gs
      )

updateGame :: GameState -> GameState
updateGame gs =
  view #_currentAnimation gs & maybe gs (\anim ->
    if view #_end anim < view #_totalTime gs
    then view #_after anim
    else gs
  )

handleEvent :: GameState -> SDL.Event -> GameState
handleEvent gs event =
  handleCommonEvent gs event
    & fromMaybe (
        if has (#_editor . _Just) gs
        then handleEditorEvent gs event
        else handleGameEvent gs event
      )

handleCommonEvent :: GameState -> SDL.Event -> Maybe GameState
handleCommonEvent gs = \case
  QuitEvent ->
    Just $ set #_quit True gs
  _ -> Nothing

handleGameEvent :: GameState -> SDL.Event -> GameState
handleGameEvent gs = \case
  MouseButtonEvent SDL.Released clickPos
    | gs & has (#_currentAnimation . _Nothing) ->
        let clickTile = floor @Double <$>
              Camera.screenToPoint (view #_camera gs) clickPos
            clickBlock = GameState.findBlockAt gs clickTile
        in maybe gs (flip blockClicked gs) clickBlock
  KeyPressEvent SDL.ScancodeE -> 
    gs 
      & set #_editor (Just . Editor.fromLevel $ view #_currentLevel gs)
      & set #_currentAnimation Nothing
  KeyPressEvent SDL.ScancodeR ->
    gs
      & set #_blockById (view (#_currentLevel . #_blockById) gs)
      & set #_currentAnimation Nothing
  _ -> gs
 
handleEditorEvent :: GameState -> SDL.Event -> GameState
-- TODO match on editor here?
handleEditorEvent gs = \case
  KeyPressEvent SDL.ScancodeE -> gs & set #_editor Nothing
  KeyPressEvent SDL.ScancodeB ->
    gs & #_editor . _Just %~ Editor.selectBounds
  KeyPressEvent (scancodeToDir -> Just dir) ->
    let keyMod = gs ^. #_keyModifier
    in if SDL.keyModifierLeftShift keyMod || SDL.keyModifierRightShift keyMod
    then gs & #_editor . _Just %~ Editor.resizeSelection dir
    else gs & #_editor . _Just %~ Editor.moveSelection dir
  MouseReleaseEvent pos ->
    let camera = fromIntegral <$> view #_camera gs
        pos' = Camera.screenToPoint @Int camera pos
    in gs & #_editor . _Just %~ Editor.selectBlockAt pos'
  _ -> gs

blockClicked :: Block Int -> GameState -> GameState
blockClicked block gs =
  case view #_behavior block of
    Block.Static -> gs
    Block.Movable dir -> gs & moveBlock block dir
    Block.Flippable normalDir flipped ->
      let blockId = view #_id block
          dir = normalDir & if flipped then negate else id
      in gs
        & moveBlock block dir
        & set (#_currentAnimation . _Just . #_after . #_blockById . at blockId . _Just . #_behavior)
            (Block.Flippable normalDir (not flipped))
    Block.Pushable -> gs

moveBlock :: Block Int -> V2 Int -> GameState -> GameState
moveBlock block dir gs =
  case Writer.runWriterT (tryPush block dir gs) of
    Just (gs', moves) ->
      let anim = animateMoves gs gs' moves
      in gs & set #_currentAnimation (Just anim)
    Nothing -> gs

tryPush :: Block Int -> V2 Int -> GameState -> WriterT (IntMap (V2 Int)) Maybe GameState
tryPush block dir gs =
  case view #_behavior block of
    Block.Static -> empty
    Block.Movable{} -> gs'
    Block.Flippable{} -> gs'
    Block.Pushable -> gs'
  where
    gs'
      | Rect.containsRect (view (#_currentLevel . #_bounds) gs) movedRect =
        allBlocks
        & toList
        & filter (not . Block.eqId block)
        & filter (view #_rect >>> Rect.intersects movedRect)
        & foldlM (\gs'' b -> tryPush b dir gs'') gs
        & fmap (set (#_blockById . at blockId . _Just) movedBlock)
        & collect (IntMap.singleton blockId $ view #_xy movedRect)
      | otherwise = empty
    allBlocks = gs & view #_blockById
    blockId = block & view #_id
    movedRect = block & view #_rect & over #_xy (+ dir)
    movedBlock = block & set #_rect movedRect
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

scancodeToDir :: SDL.Scancode -> Maybe (V2 Int)
scancodeToDir = \case
  SDL.ScancodeLeft -> Just $ V2 (-1) 0
  SDL.ScancodeRight -> Just $ V2 1 0
  SDL.ScancodeUp -> Just $ V2 0 (-1)
  SDL.ScancodeDown -> Just $ V2 0 1
  _ -> Nothing