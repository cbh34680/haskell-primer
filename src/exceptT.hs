
import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad
import System.Random
import qualified Text.Read as TR
import Data.Maybe

-- newtype ExceptT e m a = ExceptT (m (Either e a))

readMaybeInt :: IO (Maybe Int)
readMaybeInt = do
    putStr "input$ "
    s <- getLine

    return $ TR.readMaybe s


readInt :: ExceptT String IO Int
readInt = do
    mi <- lift $ readMaybeInt

    when (isNothing mi) (throwE "Error: no number")

    return $ fromJust mi



readPositive :: ExceptT String IO Int
readPositive = do
    i <- readInt

    when (i < 0) (throwE "Error: <0")

    return i


f = runExceptT readPositive
g = runExceptT (mapExceptT (fmap (fmap (+1))) readPositive)



-- EOF
