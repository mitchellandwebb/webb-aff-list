module Webb.AffList.Monad.AffList where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Webb.AffList.Internal.ListFiber as LFiber
import Webb.AffList.Internal.Node as Node
import Webb.AffList.Monad.Yield (Yield)
import Webb.AffList.Monad.Yield as Yield


{- The AffList monad represents async lists that can yield values. We yield them, 
  and compose new values that are emitted from lists that are created during
  the computation.
  
  One problem here, however, is how we are to go about 'launching' the fiber in the
  context. In the Yield context, using the List has to be done while also installing
  the list fiber as a parent. But the Aff instance allows to bypass this by
  using 'liftAff' and 'runToListFiber'. It's ... hard to be sure what to 
  do about that.

-}

data AffList a = A (Effect (Node.Node a))

instance Functor AffList where
  map = mapImpl
  
instance Apply AffList where
  apply = applyImpl
  
instance Applicative AffList where
  pure = pureImpl

runToListFiber :: forall m a. MonadEffect m => AffList a -> m (LFiber.ListFiber a)
runToListFiber (A prog) = liftEffect do 
  node <- prog
  LFiber.newListFiber node

runYieldToList :: forall a. Yield a Unit -> AffList a
runYieldToList prog = A do
  Yield.runToNode prog

-- Implement the map. Instantiate the list, take all of the values, and 
-- emit them in map form.
mapImpl :: forall a b. (a -> b) -> AffList a -> AffList b
mapImpl f list = runYieldToList do
  fiber <- launchList list
  LFiber.forEach_ fiber \a -> do 
    Yield.yield (f a)
  
-- Goes through all the functions, and then through all values, applying each
-- function to _all_ of the list. This can be dangerous -- since there are
-- effects from running the AffList.
applyImpl :: forall a b. AffList (a -> b) -> AffList a -> AffList b
applyImpl mf mx = runYieldToList do 
  fs <- launchList mf
  LFiber.forEach_ fs \f -> do 
    xs <- launchList mx
    LFiber.forEach_ xs \x -> do 
      Yield.yield (f x)
      
pureImpl :: forall a. a -> AffList a
pureImpl a = runYieldToList do Yield.yield a
    
class LaunchList m where
  launchList :: forall a. AffList a -> m (LFiber.LFiber a)
  
instance LaunchList (Yield.Yield v) where
  launchList list = do 
    fiber <- runToListFiber list
    Yield.addParent fiber -- fiber becomes a parent
    pure fiber