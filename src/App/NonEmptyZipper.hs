module App.NonEmptyZipper where

import App.Prelude

data NonEmptyZipper a = NonEmptyZipper
  { before :: [a]
  , current :: a
  , after :: [a]
  }
  deriving (Show, Generic, Functor)

singleton :: a -> NonEmptyZipper a
singleton x = NonEmptyZipper [] x []

fromNonEmpty :: NonEmpty a -> NonEmptyZipper a
fromNonEmpty (x :| xs) = NonEmptyZipper [] x xs

next :: NonEmptyZipper a -> Maybe (NonEmptyZipper a)
next (NonEmptyZipper before current after) =
  case after of
    x : afterNext -> Just $ NonEmptyZipper (current : before) x afterNext
    [] -> Nothing

prev :: NonEmptyZipper a -> Maybe (NonEmptyZipper a)
prev (NonEmptyZipper before current after) =
  case before of
    x : beforePrev -> Just $ NonEmptyZipper beforePrev x (current : after)
    [] -> Nothing

pushAfter :: a -> NonEmptyZipper a -> NonEmptyZipper a
pushAfter x (NonEmptyZipper before current after) =
  NonEmptyZipper (current : before) x after

shiftFromAfter :: NonEmptyZipper a -> Maybe (NonEmptyZipper a)
shiftFromAfter (NonEmptyZipper before _ after) =
  case after of
    x : afterNext -> Just $ NonEmptyZipper before x afterNext
    [] -> Nothing

shiftFromBefore :: NonEmptyZipper a -> Maybe (NonEmptyZipper a)
shiftFromBefore (NonEmptyZipper before _ after) =
  case before of
    x : beforePrev -> Just $ NonEmptyZipper beforePrev x after
    [] -> Nothing