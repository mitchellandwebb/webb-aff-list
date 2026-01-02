module Webb.AffList.Internal.ListFiber where

import Prelude

import Control.Monad.Error.Class (throwError)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Webb.AffList.Data.Node.Port as PV
import Webb.AffList.Internal.Node as Node
import Webb.Channel.Data.CMaybe as CMaybe



{- 
The ListFiber is to the AffList what the Fiber is to Aff. 
It provides the ability to _cancel_ the entire list computation, and to retrieve 
values from it (while values still are available). Nothing else can be done with it.
-}

newtype ListFiber a = L 
  { node :: Node.Node a
  }
  
type LFiber = ListFiber

instance Show (ListFiber a) where show _ = "ListFiber"
  
newListFiber :: forall m a. MonadEffect m => Node.Node a -> m (ListFiber a)
newListFiber node = do 
  pure $ L { node }
  
kill :: forall m a . MonadAff m => LFiber a -> m Unit
kill (L s) = Node.stop s.node

-- Receive the value. Throw if there is an error.
receive :: forall m a. MonadAff m => LFiber a -> m (CMaybe.CMaybe a)
receive (L s) = liftAff do
  pvalue <- Node.receive s.node
  case pvalue of 
    PV.Err e -> do 
      throwError e
    PV.Closed -> do 
      pure $ CMaybe.Closed
    PV.Value v -> do 
      pure $ CMaybe.Open v

-- Receive the value, including if there is an error.
receive' :: forall m a. MonadAff m => LFiber a -> m (PV.PortValue a)
receive' (L s) = liftAff do Node.receive s.node