import Control.Monad
import Data.List
import System.Random

rnds :: Int -> IO [Int]
rnds n = replicateM n $ randomRIO (0, n * 10)

shuffle :: [Int] -> IO [Int]

--shuffle xs = rnds (length xs) >>= \ys -> return $ map snd $ sort $ zip ys xs
shuffle xs = map snd . sort . flip zip xs <$> rnds (length xs)

f = shuffle [1..10]
