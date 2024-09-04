import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import qualified Text.Read as TR


step :: Int -> Except String Int
step n
    | n < 0 = throwE "Error: <0"
    | otherwise = return n


f = do
    putStr "input$ "
    s <- getLine

    case TR.readMaybe s :: Maybe Int of
        Nothing -> putStrLn "Error: no number"

        Just n -> do
            case runExcept (step n) of
                Left e -> putStrLn e
                Right _ -> print n
