import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad


getWord :: String -> MaybeT IO String
getWord title = do
    lift $ putStr ("input " ++ title ++ " $ ")
    s <- lift getLine

    -- guard (s /= "")
    when (s == "") (fail "")

    return s


getWords :: MaybeT IO String
getWords = do
    a <- getWord "1"
    b <- getWord "2"

    return (a ++ b)


loopGetWords :: IO ()
loopGetWords = do
    ms <- runMaybeT getWords
    case ms of
        Nothing -> do
            putStrLn "=> done."

        Just s  -> do
            putStrLn s
            putStrLn "=> go next"
            loopGetWords


f = loopGetWords
