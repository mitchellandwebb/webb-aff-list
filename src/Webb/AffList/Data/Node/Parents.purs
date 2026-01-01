module Webb.AffList.Data.Node.Parents where

import Prelude

import Data.Newtype (class Newtype, modify, unwrap, wrap)
import Webb.AffList.Data.Node.Parent (class Parent, Parent_)
import Webb.AffList.Data.Node.Parent as Parent
import Webb.Array as Array



newtype Parents = P (Array Parent_)

derive instance Newtype Parents _

instance Show Parents where
  show (P s) = "Parents " <> show { size: Array.size s }
  
empty :: Parents 
empty = wrap []

addParent :: forall a. Parent a => a -> Parents -> Parents
addParent a = modify $ Array.addLast (Parent.wrap a)

parents :: Parents -> Array (Parent_)
parents = unwrap

size :: Parents -> Int
size = unwrap >>> Array.size

isEmpty :: Parents -> Boolean
isEmpty = unwrap >>> Array.isEmpty
