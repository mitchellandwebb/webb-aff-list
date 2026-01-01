module Webb.AffList.Internal.Port.State where

import Prelude
import Webb.State.Prelude

import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Webb.AffList.Data.Node.Port (PortValue)
import Webb.AffList.Data.Node.Port as Port
import Webb.AffList.Data.Port.Node as Node
import Webb.Channel as Chan
import Webb.Channel.Data.CMaybe as CMaybe
import Webb.Slot (Slot)
import Webb.Slot as Slot

{-
The port's internal state. All we do with it is send values into it, 
and receive values from it. The send does not return until the value is 
sent AND a new value has been requested. The receive suspends for a value while 
ensuring the prior node has started.
-}

type PortState a = 
  { input :: Chan.Chan (PortValue a)
  , continue :: Chan.Chan Unit
  , node :: Slot Node.Node_
  }
  
newState :: forall m a. MonadEffect m => m (PortState a)
newState = do 
  input <- Chan.newChan $ Chan.finite 0
  continue <- Chan.newChan $ Chan.finite 0
  node <- Slot.newSlot "port node"
  pure { input, continue, node }
  
setNode :: forall m a n. MonadEffect m => Node.Node n =>
  PortState a -> n -> m Unit
setNode state node = do state.node := Node.wrap node
  
-- Send a value into the port. Don't return after the send -- only return when told
-- that the next value is being requested.
send :: forall m a. MonadAff m => PortState a -> PortValue a -> m Boolean
send state val = liftAff do 
  node <- aread state.node
  Node.start node
  success <- Chan.send state.input val
  if success then do
    mval <- Chan.receive state.continue
    pure $ CMaybe.isOpen mval
  else do 
    pure false
  
-- Request a value from the port. Ensure the node is started, so that it will generate values, 
-- and ensure the node, if it was suspended, gets unsuspended. 
-- Then attempt to receive a value from the port. 
receive :: forall m a. MonadAff m => PortState a -> m (PortValue a)
receive state = liftAff do 
  node <- aread state.node
  Node.start node
  _ <- Chan.trySend state.continue unit
  cmaybe <- Chan.receive state.input
  case cmaybe of 
    CMaybe.Closed -> do 
      pure Port.Closed
    CMaybe.Open val -> do 
      pure val

-- Close the port in both directions. Ensure that no matter who is waiting, they will be
-- informed of the closure.
close :: forall m a. MonadEffect m => PortState a -> m Unit
close state = do 
  Chan.close state.input
  Chan.close state.continue
      
isOpen :: forall m a. MonadEffect m => PortState a -> m Boolean
isOpen state = Chan.isOpen state.input

isClosed :: forall m a. MonadEffect m => PortState a -> m Boolean
isClosed state = Chan.isClosed state.input
      