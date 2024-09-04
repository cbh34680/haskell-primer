
import Control.Monad.Reader


test x = (`runReader` x) $ do

    a <- ask

    b <- local (+ 1) $ do
        b' <- ask
        return b'

    c <- ask

    return (a, b, c)

main = print $ test 1


