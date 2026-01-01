module Webb.AffList.Internal.Port.State where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (class MonadEffect)
import Webb.AffList.Data.Port.Node as Node
import Webb.Channel as Chan
import Webb.Slot (Slot)
import Webb.Slot as Slot


type PortState a = 
  { output :: Chan.Chan a
  , continue :: Chan.Chan Unit
  , node :: Slot Node.Node_
  }
  
newState :: forall m a. MonadEffect m => m (PortState a)
newState = do 
  output <- Chan.newChan $ Chan.finite 0
  continue <- Chan.newChan $ Chan.finite 0
  node <- Slot.newSlot "port node"
  pure { output, continue, node }