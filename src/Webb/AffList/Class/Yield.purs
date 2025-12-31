module Webb.AffList.Class.Yield where

import Prelude
import Webb.State.Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.State (StateT)
import Data.Newtype (class Newtype, wrap)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)


{- The Yield monad enables users to 'yield' values to the output, and to 
  run Aff actions, and correctly launch AffLists so that we can go through
  all values in a list and modify them before yielding them again.
-}


type State a =
  { yield :: a -> Aff Unit
  }
  
newtype Yield v a = Y (StateT (State v) Aff a)

derive newtype instance Functor (Yield v)
derive newtype instance Apply (Yield v)
derive newtype instance Applicative (Yield v)
derive newtype instance Bind (Yield v)
derive newtype instance Monad (Yield v)
derive newtype instance MonadThrow Error (Yield v)
derive newtype instance MonadError Error (Yield v)
derive newtype instance MonadEffect (Yield v)
derive newtype instance MonadAff (Yield v)
derive instance Newtype (Yield v a) _

-- Yield a value to the outer context, and pause until it's time to resume again.
yield :: forall v. v -> Yield v Unit
yield v =  wrap do
  this <- mread
  liftAff do this.yield v
