
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Debug.Trace

type Env = Int

envPlus1 :: Reader Env (Int, Int, Int, Int)
envPlus1 = do
    w <- asks (+1)
    x <- mapReader (+1) ask
    y <- local (+1) ask
    z <- withReader (+1) ask

    return (w, x, y, z)


f = do
    prt $ runReader envPlus1 10

    where
        prt r = do
            let !a = r
            putStrLn "-"
            print a
            putStrLn "------"






