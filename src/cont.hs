

--incr :: Num a => a -> (a -> b) -> b
incr a next = next (a + 1)

isodd a next = next (odd a)


--steps :: (Num a) => a -> (a -> b) -> b
steps x next = do
    incr x $ \b ->
        incr b $ \c ->
            incr c $ \d ->
                isodd d $ \e -> next e


data Cont r a = Cont { runCont :: (a -> r) -> r }


returnCont :: a -> Cont r a
returnCont x = Cont $ \f -> f x


fmapCont :: (a -> b) -> Cont r a -> Cont r b
fmapCont f m = Cont $ \c -> runCont m (c . f)


appCont :: Cont r (a -> b) -> Cont r a -> Cont r b
appCont ma mb = Cont $ \c -> runCont ma $ \b -> runCont mb (c . b)


bindCont :: Cont r a -> (a -> Cont r b) -> Cont r b
bindCont m f = Cont $ \c -> runCont m (\b -> runCont (f b) c)



instance Functor (Cont r) where
    fmap = fmapCont


instance Applicative (Cont r) where
    pure = returnCont
    (<*>) = appCont


instance Monad (Cont r) where
    (>>=) = bindCont


incrCont a = Cont $ \f -> f (a + 1)
isoddCont a = Cont $ \f -> f (odd a)


stepsCont :: Num a => a -> Cont r Bool
stepsCont n = do
    a <- incrCont n
    b <- incrCont a
    c <- incrCont b
    isoddCont c


main = do
    let x = steps 1 (\res -> res)
    print x

    let
        y = (runCont (return 1)) id
        z = (runCont (fmap (+1) (return 1))) id
        k = runCont ((pure (+1)) <*> (pure 1)) id
        l = runCont ((+) <$> pure 1 <*> pure 2) id
        m = runCont (return 1 >>= return . (+1)) id
        n = runCont (stepsCont 1) id

    print y
    print z
    print k
    print l
    print m
    print n

    putStrLn "done."



-- EOF
