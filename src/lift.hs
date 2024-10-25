module Main where

import Control.Monad.Trans.State
import Control.Monad.Trans


lft :: Monad m => m a -> StateT s m a

{-
lft act = StateT $ \s -> do
                    x <- act
                    return (x, s)
-}

lft act = StateT $ \s -> (, s) <$> act


prt :: Int -> StateT Int IO ()
prt n = StateT $ \s -> do
                    putStrLn ("VAL is " ++ show n)
                    return ((), s)


step :: StateT Int IO String
step = do
    get >>= prt

    prev <- get
    modify (+1)

    lft $ putStrLn ("prev is " ++ show prev)

    show <$> get


main = do
    r <- runStateT step 1
    print r

    putStrLn "###"
    putStrLn "### done."
    putStrLn "###"




-- EOF
