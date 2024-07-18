-- hugs98

import Debug.Trace
import Control.Applicative

data MyState s a = NewMyState { runState :: s -> (a, s) }

instance Monad (MyState s) where
    return x = NewMyState $ \s -> (x, s)
    (NewMyState h) >>= f = NewMyState $ \s ->
        let (a, newState) = h s
            (NewMyState g) = f a
        in  g newState

push x = NewMyState $ \xs -> ((), (x:xs))
pop    = NewMyState $ \(x:xs) -> (x, xs)


f = runState (return 1 >>= push >> pop) [4..10]
