import Control.Monad
import Data.Char

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' _ [] = return []
mapM' f (x:xs) = f x >>= \y -> mapM' f xs >>= \ys -> return (y:ys)

f = mapM' id ["ab", "cd", "ef"]
g = mapM' Right ["ab", "cd", "ef"]

mapM'' :: (a -> [b]) -> [a] -> [[b]]
mapM'' _ [] = [[]]
mapM'' f (x:xs) = concatMap (\y -> (concatMap (\ys -> (return (y:ys))) (mapM'' f xs))) (f x)

f' = mapM'' id ["ab", "cd", "ef"]
