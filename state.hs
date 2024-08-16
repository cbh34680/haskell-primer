data State s a = GenState { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap f m = GenState $ \s -> let (a', s') = runState m s in (f a', s)

instance Applicative (State s) where
    pure = genState
    m <*> n = GenState $ \s ->
        let (f, s') = runState m s
            (x, s'') = runState n s'
        in (f x, s'')

instance Monad (State s) where
    m >>= f = GenState $ \s ->
        let (x, s') = runState m s in runState (f x) s'

genState x = GenState $ \s -> (x, s)

push :: a -> State [a] ()
push x = GenState $ \s -> ((), x:s)

pop :: State [a] a
pop = GenState $ \(x:xs) -> (x, xs)


f = runState (genState 1) []

g = runState (pure (+1) <*> pure 1) []

h = runState step []
    where
        step = pure 1 >>= return . (+1) >>= push >> pop

-- EOF
