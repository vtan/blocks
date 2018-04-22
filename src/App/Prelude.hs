{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module App.Prelude (module X) where

import Prelude as X
import Control.Applicative as X
import Control.Arrow as X
import Control.Lens.Operators as X
import Data.Foldable as X
import Data.Function as X
import Data.Maybe as X

import Control.Lens as X
  (view, set, over, has, at, _Just, _Nothing)
import Control.Monad.Writer.CPS as X
  (WriterT)
import Data.Generics.Product as X
  (field)
import Data.HashMap.Strict as X
  (HashMap)
import Data.IntMap.Strict as X
  (IntMap)
import Data.Semigroup as X
  ((<>))
import Debug.Trace as X
  (traceShowId)
import GHC.Generics as X
  (Generic)
import Linear as X
  (V2(..), V4(..), (*^))

import qualified Data.Generics.Product (HasField, field)
import qualified GHC.OverloadedLabels (IsLabel(..))
instance (Data.Generics.Product.HasField field s t a b, Functor f, sft ~ (s -> f t)) =>
  GHC.OverloadedLabels.IsLabel field ((a -> f b) -> sft) where
  fromLabel = Data.Generics.Product.field @field
