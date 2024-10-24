import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import qualified Text.Read as TR

main = do
    r <- runExceptT (doSomething `catchE` handleException)
    print r


handleException :: String -> ExceptT String IO Int
handleException msg = do
    lift $ putStrLn msg
    throwE $ "catch: " ++ msg


doSomething :: ExceptT String IO Int
doSomething = do
    n <- readInt

    if n < 10 then throwE "< 10" else return n




readInt :: ExceptT String IO Int
readInt = do
    s <- liftIO getLine

    n <- ExceptT $ return $ case readMaybeInt s of
                Just n -> Right n
                Nothing -> Left $ "err: " ++ s

    return n


readMaybeInt :: String -> Maybe Int
readMaybeInt s = TR.readMaybe s

