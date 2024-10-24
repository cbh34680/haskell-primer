
import Control.Monad
import Data.Monoid
import Control.Monad.Trans.Writer


plus :: Int -> Writer (Sum Int) ()
plus x = tell (Sum x)


f :: [Int] -> Writer (Sum Int) ()
f xs = do
    forM_ xs plus


main = do
    print $ runWriter (f [1..10])
