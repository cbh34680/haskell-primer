

data Cont r a = Cont { runCont :: (a -> r) -> r }

returnCont a = Cont $ \next -> next a

--
fmapCont f m = Cont $ \next -> (runCont m) $ \a -> next (f a)

instance Functor (Cont r) where
    fmap = fmapCont

testFmap = fmap (+1) (returnCont 1) `runCont` id

--
mf `appCont` ma = Cont $ \next ->
    (runCont mf) $ \f ->
        (runCont ma) $ \a ->
            next (f a)

instance Applicative (Cont r) where
    pure = returnCont
    (<*>) = appCont

testApplicative = (pure (+1) <*> pure (1)) `runCont` id

--
ma `bindCont` f = Cont $ \next ->
    (runCont ma) $ \a ->
        (runCont (f a)) $ \b ->
            next b

instance Monad (Cont r) where
    (>>=) = bindCont

testMonad = (return 1 >>= return . (+1)) `runCont` id


main = do
    let
        tf = testFmap
        ta = testApplicative
        tm = testMonad

    print [tf, ta, tm]


-- EOF

