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
import qualified SDL as SDL

import App.Block (Block)
import App.GameState (GameState)

pattern QuitEvent :: SDL.Event
pattern QuitEvent <-
  SDL.Event { SDL.eventPayload = SDL.QuitEvent }

pattern KeyReleaseEvent :: SDL.Scancode -> SDL.Event
pattern KeyReleaseEvent scancode <-
  SDL.Event 
    { SDL.eventPayload = SDL.KeyboardEvent 
      ( SDL.KeyboardEventData
        { SDL.keyboardEventKeyMotion = SDL.Released
        , SDL.keyboardEventKeysym = SDL.Keysym { SDL.keysymScancode = scancode }
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
  KeyReleaseEvent SDL.ScancodeE -> 
    gs 
      & set #_editor (Just . Editor.fromLevel $ view #_currentLevel gs)
      & set #_currentAnimation Nothing
  KeyReleaseEvent SDL.ScancodeR ->
    gs
      & set #_blockById (view (#_currentLevel . #_blockById) gs)
      & set #_currentAnimation Nothing
  _ -> gs
 
handleEditorEvent :: GameState -> SDL.Event -> GameState
handleEditorEvent gs = \case
  KeyReleaseEvent SDL.ScancodeE -> gs & set #_editor Nothing
  MouseButtonEvent SDL.Pressed pos ->
    let camera = fromIntegral <$> view #_camera gs
        clickedTile = floor @Float <$> Camera.screenToPoint @Int camera pos
    in case gs
      & view (#_editor. _Just . #_level . #_blockById)
      & find (\block -> Rect.contains (view #_rect block) clickedTile)
    of
      Just block -> gs
        & set (#_editor . _Just . #_currentAction) (Just $ Editor.MoveBlock block clickedTile)
      Nothing -> gs
  MouseButtonEvent SDL.Released pos ->
    let camera = fromIntegral <$> view #_camera gs
        clickedTile = floor @Float <$> Camera.screenToPoint @Int camera pos
    in case preview (#_editor . _Just . #_currentAction . _Just) gs of
      Just (Editor.MoveBlock block grabbedTile) ->
        let blockId = view #_id block
            block' = block & over (#_rect . #_xy) (+ (clickedTile - grabbedTile))
        in gs
          & set (#_editor . _Just . #_level . #_blockById . at blockId . _Just) block'
          & set (#_editor . _Just . #_currentAction) Nothing
      Nothing -> gs
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