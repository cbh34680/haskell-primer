import Control.Monad.Trans
import System.IO
import Control.Exception
import Data.Char
import Data.List
import Text.Read

import qualified Data.List.Split as DLS

data Env = Env { unStart :: Int, unEnd :: Int } deriving Show

-- "start=1\nend=2"

loadEnv :: FilePath -> IO (Maybe Env)
loadEnv path = do
    genEnv `catch` (\(SomeException e) -> return Nothing)
    where
        genEnv = do
            confs <- map arr2pair . str2arrs <$> readFile path

            let toInt key = lookup key confs >>= readMaybe

            return $ Env <$> toInt "start" <*> toInt "end"

        str2arrs = filter (\xs -> and ([cond1, cond2] <*> [xs])) . map (map trim . DLS.splitOn "=") . lines
        arr2pair = (,) <$> (!! 0) <*> (!! 1)

        cond1 = (== 2) . length
        cond2 = ('#' /=) . head . (!! 0)
        trim = dropWhileEnd isSpace . dropWhile isSpace

main = do
    me <- loadEnv "env.conf"
    case me of
        Just e -> putStrLn $ show (unStart e)
    



-- EOF
