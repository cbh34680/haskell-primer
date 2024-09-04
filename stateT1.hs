import Control.Monad.State
import Control.Monad.Identity


return' x = StateT $ \s -> Identity (x, s)

runState' m s = runIdentity $ runStateT m s


main = do
    let st = return' 1
    print $ runState' st ()

