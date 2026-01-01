module Webb.AffList.Data.Node.Port where

import Prelude

import Effect.Aff (Aff, Error)



class OutputPort a d | a -> d where
  send :: a -> (PortValue d) -> Aff Unit

newtype OutputPort_ a0 = OutputPort__ (forall r. (forall z. OutputPort z a0 => z -> r) -> r)

wrapOutput :: forall z a0. OutputPort z a0 => z -> OutputPort_ a0
wrapOutput z = OutputPort__ (_ $ z)

instance OutputPort (OutputPort_ a0) a0 where 
  send (OutputPort__ run) = run send
  

class InputPort a d | a -> d where
  start :: a -> Aff Unit -- ensure start of the the input port's source.
  receive :: a -> Aff (PortValue d)

newtype InputPort_ a0 = InputPort__ (forall r. (forall z. InputPort z a0 => z -> r) -> r)

wrapInput :: forall z a0. InputPort z a0 => z -> InputPort_ a0
wrapInput z = InputPort__ (_ $ z)

instance InputPort (InputPort_ a0) a0 where 
  start (InputPort__ run) = run start
  receive (InputPort__ run) = run receive

-- A port can deliver notice of closure, of value, or of error.
data PortValue a = Closed | Value a | Err Error