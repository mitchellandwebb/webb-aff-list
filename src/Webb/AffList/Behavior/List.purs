module Webb.AffList.Behavior.List where

import Prelude
import Webb.State.Prelude

import Data.Foldable as Fold
import Data.Maybe (Maybe(..))
import Webb.AffList.Internal.ListFiber as LFiber
import Webb.AffList.Monad.AffList (AffList, launchList, runYieldToList)
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

-- Take all items satisfying the condition, but no more than that. Note that this 
-- necessarily means the first non-matching item will be computed, even if this has effects.
takeWhile :: forall a. (a -> Boolean) -> AffList a -> AffList a
takeWhile f mx = runYieldToList do 
  xs <- launchList mx
  LFiber.while_ xs shouldTake $ take_
  where
  continue = localEffect do newShowRef true
  shouldTake = aread continue
  take_ x = do 
    continue := f x
    whenM (aread continue) do 
      yield x
      
-- We drop while items exist to be dropped, and then emit the rest.
dropWhile :: forall a. (a -> Boolean) -> AffList a -> AffList a
dropWhile f mx = runYieldToList do 
  xs <- launchList mx
  LFiber.while_ xs shouldDrop $ drop_
  yieldLast
  LFiber.forEach_ xs yield

  where
  last = localEffect do newShowRef Nothing
  continue = localEffect do newShowRef true
  shouldDrop = aread continue
  drop_ x = do 
    continue := f x
    last := Just x
  yieldLast = do 
    mval <- aread last
    Fold.for_ mval yield
  