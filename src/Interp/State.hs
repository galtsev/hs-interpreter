module Interp.State where

newtype State s a = State (s -> (s, a))

runState:: State s a -> s -> (s, a)
runState (State a) tb = a tb

instance Functor (State s) where
    fmap f (State v) = State $ go
        where
            go tb = let (ntb, r) = v tb in (ntb, f r)

instance Applicative (State s) where
    pure r = State $ \tb -> (tb, r)
    (State fn) <*> (State arg) = State $ go
        where
            go tb =
                let 
                    (tb1, fn1) = fn tb
                    (tb2, arg1) = arg tb1
                in
                    (tb2, fn1 arg1)

instance Monad (State s) where
    (State a) >>= f = State $ go
        where
            go tb =
                let
                    (tb1, a1) = a tb
                in
                    runState (f a1) tb1

get:: State s s
get = State $ \s -> (s,s)

set:: s -> State s ()
set s = State $ \_ -> (s, ())

update:: (s -> s) -> State s ()
update f = State $ \s -> (f s, ())
