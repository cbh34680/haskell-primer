import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import Control.Monad
import qualified Text.Read as TR


readInt :: String -> Maybe Int
readInt = TR.readMaybe


inputNum :: MaybeT IO Int

--inputNum = MaybeT $ fmap readInt getLine
inputNum = do
    s <- lift $ getLine
    hoistMaybe $ readInt s


g :: MaybeT IO Int
g = do
    lift $ putStr "input> "
    n <- inputNum

    guard (n > 0)

    lift $ putStrLn ("accept number is " ++ show n)

    return n

f = runMaybeT g
f' = runMaybeT (fmap (+1) g)


