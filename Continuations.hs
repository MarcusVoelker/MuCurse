module Continuations where

import Control.Monad

data DCont r e a = DCont {run :: (a -> r) -> (e -> r) -> r}

chain :: DCont r e a -> (a -> DCont r e b) -> DCont r e b
chain c f = DCont (\btr etr -> run c (\x -> run (f x) btr etr) etr)

weakChain :: DCont r e a -> (a -> DCont r e a) -> DCont r e a
weakChain c f = DCont (\btr etr -> run c (\x -> run (f x) btr (\_ -> btr x)) etr)

exceptChain :: DCont r e a -> (a -> DCont r e b) -> (a -> b) -> DCont r e b
exceptChain c f e = DCont (\btr etr -> run c (\x -> run (f x) btr (\_ -> btr $ e x)) etr)

branch :: DCont r e a -> DCont r e a -> DCont r e a
branch c1 c2 = DCont (\atr etr -> run c1  atr (\_ -> run c2 atr etr))

returnValue :: a -> DCont r e a
returnValue x = DCont (\f _ -> f x)

throw :: e -> DCont r e a
throw x = DCont (\_ g -> g x)

instance Monad (DCont r e) where
    return = returnValue
    (>>=) = chain

instance Functor (DCont r e) where
    fmap = liftM
