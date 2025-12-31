module Webb.AffList.Class.AffList where

import Prelude


{- The AffList monad represents async lists that can yield values. We yield them, 
  and compose new values that are emitted from lists that are created during
  the computation.
-}