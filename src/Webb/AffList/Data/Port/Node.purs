module Webb.AffList.Data.Port.Node where

import Prelude

import Effect.Aff (Aff)


class Node a where
  start :: a -> Aff Unit

newtype Node_ = Node__ (forall r. (forall z. Node z => z -> r) -> r)

instance Show (Node_) where show _ = "Node"

wrap :: forall z. Node z => z -> Node_
wrap z = Node__ (_ $ z)

instance Node (Node_) where 
  start (Node__ run) = run start
