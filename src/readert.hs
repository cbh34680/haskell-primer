import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import System.IO
import Control.Exception
import Data.Char
import Data.List

import qualified Text.Read as TR
import qualified Data.List.Split as DLS

data Env = Env { unStart :: Int, unEnd :: Int } deriving Show

-- "start=1\nend=2"

loadEnv :: FilePath -> IO (Maybe Env)
loadEnv path = do
    genEnv `catch` (\(SomeException e) -> return Nothing)
    where
        genEnv = do
            confs <- map arr2pair . str2arrs <$> readFile path

            let toInt key = lookup key confs >>= TR.readMaybe

            return $ Env <$> toInt "start" <*> toInt "end"

        str2arrs = filter (\xs -> and ([cond1, cond2] <*> [xs])) .
                    map (map trim . DLS.splitOn "=") . lines

        arr2pair = (,) <$> (!! 0) <*> (!! 1)

        cond1 = (== 2) . length
        cond2 = ('#' /=) . head . (!! 0)
        trim = dropWhileEnd isSpace . dropWhile isSpace


printRange :: ReaderT Env IO ()
printRange = do
    s <- asks unStart
    e <- asks unEnd

    lift $ print [s .. e]

    return ()

main = do
    me <- loadEnv "env.conf"

    case me of
        Just env -> runReaderT printRange env
        _ -> return ()




-- EOF
