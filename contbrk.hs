import Control.Monad.Trans.Cont
import Control.Monad

steps :: Int -> Cont Int Int
steps n = do
    callCC $ \exit -> do
        when (n == 5) (exit 100)

        if n > 0
            then steps (n - 1)
            else return 1010

main = do
    let x = runCont (steps 10) id

    print x
    putStrLn "done."
