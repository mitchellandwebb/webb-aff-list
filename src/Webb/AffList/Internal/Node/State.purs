module Webb.AffList.Internal.Node.State where

import Prelude

import Webb.AffList.Data.Node.Parents (Parents)
import Webb.AffList.Data.Node.Port (InputPort_)
import Webb.State.Prelude (ShowRef)
import Webb.Thread (Thread)




type NodeState = 
  { thread :: Thread
  , parents :: ShowRef Parents
  , port :: Slot InputPort_
  }