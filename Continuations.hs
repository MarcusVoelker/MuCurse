module Continuations where

import Control.Monad.Cont

data DCont r e a = DCont {run :: ((a -> r) -> (e -> r) -> r)}

chain :: DCont r e a -> (a -> DCont r e b) -> DCont r e b
chain c f = DCont (\btr etr -> (run c) (\x -> (run (f x)) btr etr) etr)

returnValue :: a -> DCont r e a
returnValue x = DCont (\f g -> f x)

returnError :: e -> DCont r e a
returnError x = DCont (\f g -> g x)

instance Monad (DCont r e) where
    return = returnValue
    (>>=) = chain


