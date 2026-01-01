module Webb.AffList.Data.Node.Parent where

import Prelude

import Effect.Aff (Aff)



class Parent a where
  cancel :: a -> Aff Unit

newtype Parent_ = Parent__ (forall r. (forall z. Parent z => z -> r) -> r)

wrap :: forall z. Parent z => z -> Parent_
wrap z = Parent__ (_ $ z)

instance Parent (Parent_) where 
  cancel (Parent__ run) = run cancel
