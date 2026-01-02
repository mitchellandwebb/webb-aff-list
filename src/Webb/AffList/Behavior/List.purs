module Webb.AffList.Behavior.List where

import Prelude
import Webb.State.Prelude

import Webb.AffList.Internal.ListFiber as LFiber
import Webb.AffList.Monad.AffList (AffList, runYieldToList, launchList)
import Webb.AffList.Monad.AffList as AffList
import Webb.AffList.Monad.Yield (yield)
import Webb.Stateful (localEffect)


{- Add common list-like behaviors for the AffList. -}


-- Take only a few values before stopping.
take :: forall a. Int -> AffList a -> AffList a
take n mx = runYieldToList do
  xs <- launchList mx
  LFiber.while_ xs shouldTake $ \x -> do 
    take_ x

  where
  count = localEffect do newShowRef 0
  take_ x = do 
    (_ + 1) :> count
    yield x
  shouldTake = do 
    i <- aread count
    pure $ i < n

-- Drop only the first 'n' values, and then emit the rest.
drop :: forall a. Int -> AffList a -> AffList a
drop n mx = runYieldToList do 
  xs <- launchList mx
  LFiber.while_ xs shouldDrop $ \x -> do 
    drop_ x
  LFiber.forEach_ xs yield
  where
  count = localEffect do newShowRef 0
  shouldDrop = do 
    i <- aread count
    pure $ i < n
  drop_ _x = do 
    (_ + 1) :> count
