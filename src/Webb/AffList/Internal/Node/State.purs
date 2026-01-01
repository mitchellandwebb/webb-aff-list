module Webb.AffList.Internal.Node.State where

import Prelude
import Webb.State.Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Webb.AffList.Data.Node.Parent (class Parent, Parent_)
import Webb.AffList.Data.Node.Parents (Parents)
import Webb.AffList.Data.Node.Parents as Parents
import Webb.AffList.Data.Node.Port (class OutputPort, OutputPort_)
import Webb.AffList.Data.Node.Port as Port
import Webb.Slot (Slot, newSlot)
import Webb.Thread (Thread)
import Webb.Thread as Thread




type NodeState a = 
  { thread :: Thread
  , parents :: ShowRef Parents
  , port :: Slot (OutputPort_ a) -- We have an output port, from the perspective of the node.
  }
  
new :: forall m a. MonadEffect m => m (NodeState a)
new = do
  thread <- Thread.newThread
  parents' <- newShowRef Parents.empty
  port' <- newSlot "node port"
  pure { thread, parents: parents', port: port' }
  
setPort :: forall o b m. MonadEffect m => OutputPort o b => NodeState b -> o -> m Unit
setPort node output = do node.port := Port.wrapOutput output

port :: forall a m. MonadEffect m => NodeState a -> m (OutputPort_ a)
port state = do 
  p <- aread state.port
  pure p
  
start :: forall a m. MonadAff m => NodeState a -> m Unit
start state = do Thread.start state.thread

stop :: forall a m. MonadAff m => NodeState a -> m Unit
stop state = do Thread.stop state.thread

addParent :: forall a p m. MonadEffect m => Parent p =>
  NodeState a -> p -> m Unit
addParent state p = do Parents.addParent p :> state.parents

parents :: forall a m. MonadEffect m =>
  NodeState a -> m (Array Parent_)
parents state = do Parents.parents <: state.parents

parentCount :: forall a m. MonadEffect m =>
  NodeState a -> m Int
parentCount state = do Parents.size <: state.parents

