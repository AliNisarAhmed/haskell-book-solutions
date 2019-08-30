module IdTraverse where

import Data.Functor.Identity
import Data.Monoid
import Data.Functor.Constant

edgemap f t = runIdentity $ traverse (Identity . f) t