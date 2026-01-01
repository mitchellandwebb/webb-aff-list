module Webb.AffList.Internal.Node where

import Prelude
import Webb.State.Prelude

import Data.Newtype (class Newtype)
import Effect.Class (class MonadEffect)
import Webb.AffList.Internal.Node.Parents (Parents)
import Webb.AffList.Internal.Node.Port (Port)
import Webb.AffList.Internal.Node.Port as Port
import Webb.AffList.Internal.Node.Program (Program)
import Webb.AffList.Internal.Node.State (class LateNode, NodeState)
import Webb.AffList.Internal.Node.State as State

newtype Node a = N (NodeState a)

instance LateNode (Node a) where
  closePort (N s) = Port.close s
  errorPort (N s) e = void (Port.error s e)

derive instance Newtype (Node a) _

instance Show (Node a) where
  show (N s) = show
    { thread: s.thread
    , parents: s.parents
    , deliver: s.deliver
    , mutex: s.mutex
    , started: s.started
    }

newNode :: forall m a. MonadEffect m => m (Node a)
newNode = do
  state' <- State.new
  let node = N state'
  state'.node := State.wrap node
  pure node

port :: forall a m. Monad m => Node a -> m (Port a)
port (N s) = pure s

program :: forall a m. Monad m => Node a -> m (Program a)
program (N s) = pure s

parents :: forall a m. Monad m => Node a -> m (Parents a)
parents (N s) = pure s

state :: forall a m. Monad m => Node a -> m (NodeState a)
state (N s) = pure s