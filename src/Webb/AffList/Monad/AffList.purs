module Webb.AffList.Monad.AffList where

import Prelude

import Effect (Effect)
import Webb.AffList.Internal.Node as Node


{- The AffList monad represents async lists that can yield values. We yield them, 
  and compose new values that are emitted from lists that are created during
  the computation.
-}

data AffList a = A (Effect (Node.Node a))