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

get :: State s s
get = GenState $ \s -> (s, s)

put :: s -> State s ()
put s = GenState $ \_ -> ((), s)

gets :: (s -> a) -> State s a
-- gets f = GenState $ \s -> (f s, s)
gets f = do
    s <- get
    return $ f s

modify :: (s -> s) -> State s ()
-- modify f = GenState $ \s -> ((), f s)
modify f = do
    s <- get
    put $ f s


mapState :: ((a, s) -> (b, s)) -> State s a -> State s b
mapState f m = GenState $ f . runState m


f = runState (genState 1) []

g = runState (pure (+1) <*> pure 1) []

h = runState step []
    where
        step = pure 1 >>= return . (+1) >>= push >> pop

i = runState (mapState (\(a, s) -> (a+1, s+1)) (pure 1)) 1








-- EOF
