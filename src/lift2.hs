import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader

--
f1 :: StateT Int IO String

f1 = StateT $ \s -> do
        putStr "input1$ "
        cs <- getLine

        return (cs, s + length cs)


g1 :: String -> StateT Int IO String

g1 cs = do
    lift $ putStrLn $ concat ["arg1 is [", cs, "]"]

    modify (+1)

    return $ cs ++ "!"

--
f2 :: StateT Int (ReaderT Int IO) String

f2 = StateT $ \s -> do
        cs <- ReaderT $ \r -> do
                putStr "input$ "
                cs' <- getLine
                return cs'

        return (cs, s + length cs)


g2 :: String -> StateT Int (ReaderT Int IO) String

g2 cs = do
    lift . lift $ putStrLn $ concat ["arg2 is [", cs, "]"]

    modify (+1)

    return $ cs ++ "!"


--
f3 :: StateT Int (ReaderT Int IO) String

f3 = StateT $ \s ->
        ReaderT $ \r -> do
                putStr "input3$ "
                cs <- getLine

                return (cs, s + length cs)


g3 :: String -> StateT Int (ReaderT Int IO) String

g3 cs = do
    liftIO $ putStrLn $ concat ["arg3 is [", cs, "]"]

    modify (+1)

    return $ cs ++ "!"


--
f4 :: StateT Int (ReaderT Int IO) String

f4 = StateT $ \s ->
        ReaderT $ \r ->
                putStr "input3$ " >>

                    --(\cs -> (cs, s + length cs)) <$> getLine
                    ((,) <$> id <*> (+s) . length) <$> getLine


g4 :: String -> StateT Int (ReaderT Int IO) String

g4 cs = do
    r <- lift $ ask

    liftIO $ putStrLn $ concat ["arg4 is [", cs, "] r=", show r]

    modify (+1)

    return $ cs ++ "!"

--
main = do
    {-
    x1 <- runStateT (f1 >>= g1) 200
    print x1
    putStrLn ""

    x2 <- runReaderT (runStateT (f2 >>= g2) 200) 300
    print x2
    putStrLn ""

    x3 <- runReaderT (runStateT (f3 >>= g3) 200) 300
    print x3
    putStrLn ""
    -}

    x4 <- runReaderT (runStateT (f4 >>= g4) 200) 300
    print x4
    putStrLn ""

    putStrLn "done."


-- EOF
