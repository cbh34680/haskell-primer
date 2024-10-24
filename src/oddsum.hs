import Data.Bool
import Control.Applicative


guardOdd :: Alternative m => Int -> m Int
guardOdd x = bool empty (pure x) (odd x)


oddSum :: [Int] -> Maybe Int
oddSum [] = return 0
oddSum (x:xs) = (+) <$> guardOdd x <*> oddSum xs


f = oddSum [1..10]
g = oddSum [1,3..10]
