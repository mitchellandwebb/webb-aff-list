module Webb.AffList 
( module P
, AList
)
where

import Prelude

import Webb.AffList.Monad.AffList (AffList, runYieldToList, class LaunchList, launchList) as P
import Webb.AffList.Monad.Yield (Yield, addParent, finally, onCancel, asAff) as P
import Webb.AffList.Behavior.List (take, drop, takeWhile, dropWhile, fromFoldable, foldl, scanl, filter, reject, groupN) as P
import Webb.AffList.Internal.ListFiber (ListFiber, LFiber, kill, receive, receive', forEach_, while_) as P

import Webb.AffList.Monad.AffList (AffList)

{-
The AffList provides all methods needed to work with the AffList.
-}

type AList = AffList