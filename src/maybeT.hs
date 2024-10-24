
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class
import qualified Text.Read as TR

readInt :: String -> Maybe Int
readInt = TR.readMaybe


f = runMaybeT g

-- MaybeT ... IO (Maybe Int) --> MaybeT IO Int

g :: MaybeT IO Int
g = do
    lift $ putStr "input a> "
    a <- MaybeT $ fmap readInt getLine

    lift $ putStr "input b> "
    b <- MaybeT $ fmap readInt getLine

    return (a + b)




