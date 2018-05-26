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

import App.Block (Block(..))
import App.Editor (Editor)
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

animationLength :: Float
animationLength = 0.2

update :: Float -> [SDL.Event] -> GameState -> GameState
update lastFrameTime events =
  over #totalTime (+ lastFrameTime)
  >>> (\gs -> foldl' handleEvent gs events)
  >>> (\gs  -> if has (#editor . _Nothing) gs
        then updateGame gs
        else gs
      )

updateGame :: GameState -> GameState
updateGame gs =
  view #currentAnimation gs & maybe gs (\anim ->
    if view #end anim < view #totalTime gs
    then view #after anim
    else gs
  )

handleEvent :: GameState -> SDL.Event -> GameState
handleEvent gs event =
  handleCommonEvent gs event
    & fromMaybe (
        case gs ^. #editor of
          Just editor -> handleEditorEvent editor gs event
          Nothing -> handleGameEvent gs event
      )

handleCommonEvent :: GameState -> SDL.Event -> Maybe GameState
handleCommonEvent gs = \case
  QuitEvent -> Just $ set #quit True gs
  _ -> Nothing

handleGameEvent :: GameState -> SDL.Event -> GameState
handleGameEvent gs = \case
  MouseReleaseEvent clickPos
    | gs & has (#currentAnimation . _Nothing) ->
        let clickTile = floor @Double <$>
              Camera.screenToPoint (view #camera gs) clickPos
            clickBlock = GameState.findBlockAt gs clickTile
        in maybe gs (flip blockClicked gs) clickBlock
  KeyPressEvent SDL.ScancodeE -> 
    gs 
      & set #editor (Just . Editor.fromLevel $ view #currentLevel gs)
      & set #currentAnimation Nothing
  KeyPressEvent SDL.ScancodeR ->
    gs
      & set #blockById (view (#currentLevel . #blockById) gs)
      & set #currentAnimation Nothing
  _ -> gs
 
handleEditorEvent :: Editor -> GameState -> SDL.Event -> GameState
handleEditorEvent editor gs = \case
  KeyPressEvent SDL.ScancodeE ->
    let level = editor ^. #level
    in gs
      & #editor .~ Nothing
      & #currentLevel .~ level
      & #blockById .~ (level ^. #blockById)
  KeyPressEvent SDL.ScancodeB ->
    gs & #editor . _Just %~ Editor.selectBounds
  KeyPressEvent (scancodeToDir -> Just dir) ->
    let keyMod = gs ^. #keyModifier
    in if
      | SDL.keyModifierLeftShift keyMod || SDL.keyModifierRightShift keyMod ->
        gs & #editor . _Just %~ Editor.resizeSelection dir
      | SDL.keyModifierLeftCtrl keyMod || SDL.keyModifierRightCtrl keyMod ->
        gs & #editor . _Just %~ Editor.orientSelection dir
      | otherwise ->
        gs & #editor . _Just %~ Editor.moveSelection dir
  KeyPressEvent (scancodeToBehavior -> Just behavior) ->
    gs & #editor . _Just %~ Editor.setSelectionBehavior behavior
  KeyPressEvent SDL.ScancodeKPPlus ->
    gs & #editor . _Just %~ Editor.createBlock
  KeyPressEvent SDL.ScancodeKPMinus ->
    gs & #editor . _Just %~ Editor.deleteSelection
  MouseReleaseEvent pos ->
    let camera = fromIntegral <$> view #camera gs
        pos' = Camera.screenToPoint @Int camera pos
    in gs & #editor . _Just %~ Editor.selectTileAt pos'
  _ -> gs

blockClicked :: Block Int -> GameState -> GameState
blockClicked block@Block{ orientation, behavior } gs =
  case behavior of
    Block.Movable -> gs & moveBlock block orientation
    Block.Flippable flipped ->
      let blockId = view #uid block
          currentDir = orientation & if flipped then negate else id
      in gs
        & moveBlock block currentDir
        & set (#currentAnimation . _Just . #after . #blockById . at blockId . _Just . #behavior)
            (Block.Flippable $ not flipped)
    Block.Static -> gs
    Block.Collectable -> gs
    Block.Pushable -> gs

moveBlock :: Block Int -> V2 Int -> GameState -> GameState
moveBlock block dir gs =
  case Writer.runWriterT (tryPush block dir gs) of
    Just (gs', moves) ->
      let anim = animateMoves gs gs' moves
      in gs & set #currentAnimation (Just anim)
    Nothing -> gs

tryPush :: Block Int -> V2 Int -> GameState -> WriterT (IntMap (V2 Int)) Maybe GameState
tryPush block dir gs =
  case view #behavior block of
    Block.Static -> empty
    Block.Movable{} -> gs'
    Block.Flippable{} -> gs'
    Block.Pushable -> gs'
    Block.Collectable -> gs'
  where
    gs'
      | Rect.containsRect (view (#currentLevel . #bounds) gs) movedRect =
        allBlocks
        & toList
        & filter (not . Block.eqUid block)
        & filter (view #rect >>> Rect.intersects movedRect)
        & foldlM (\gs'' b -> tryPush b dir gs'') gs
        & fmap (set (#blockById . at blockId . _Just) movedBlock)
        & collect (IntMap.singleton blockId $ view #xy movedRect)
      | otherwise = empty
    allBlocks = gs & view #blockById
    blockId = block & view #uid
    movedRect = block & view #rect & over #xy (+ dir)
    movedBlock = block & set #rect movedRect
    collect w x = x <* Writer.tell w

animateMoves :: GameState -> GameState -> IntMap (V2 Int) -> GameState.Animation
animateMoves gs gs' moves =
  GameState.Animation
    { GameState.start = time
    , GameState.end = time + animationLength
    , GameState.movingBlocksById = movingBlocks
    , GameState.otherBlocksById = otherBlocks
    , GameState.after = gs'
    }
  where
    blocks = view #blockById gs
    time = view #totalTime gs
    movingBlocks = toList $ IntMap.intersectionWith (,) blocks moves
    otherBlocks = toList $ IntMap.difference blocks moves

scancodeToDir :: SDL.Scancode -> Maybe (V2 Int)
scancodeToDir = \case
  SDL.ScancodeLeft -> Just $ V2 (-1) 0
  SDL.ScancodeRight -> Just $ V2 1 0
  SDL.ScancodeUp -> Just $ V2 0 (-1)
  SDL.ScancodeDown -> Just $ V2 0 1
  _ -> Nothing

scancodeToBehavior :: SDL.Scancode -> Maybe Block.Behavior
scancodeToBehavior = \case
  SDL.ScancodeKP0 -> Just Block.Static
  SDL.ScancodeKP1 -> Just Block.Pushable
  SDL.ScancodeKP2 -> Just Block.Movable
  SDL.ScancodeKP3 -> Just $ Block.Flippable { Block.flipped = False} 
  SDL.ScancodeKP4 -> Just Block.Collectable
  _ -> Nothing