import Control.Monad
import Control.Monad.ST
import Data.STRef

{-
ghci> :t runST
runST :: (forall s. ST s a) -> a
ghci> :t newSTRef
newSTRef :: a -> ST s (STRef s a)
-}

sum' :: [Int] -> Int
sum' xs = runST step
    where
        step :: ST s Int
        step = do
            ref <- newSTRef 0

            forM_ xs $ \i -> do
                modifySTRef ref (+i)

            readSTRef ref


f = sum' [1..10]

