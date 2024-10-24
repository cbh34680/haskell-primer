import Control.Exception
import Data.Maybe
import System.Environment

mapM' f = foldr g (return [])
    where
        g a b = do
            x <- f a
            xs <- b
            return $ x : xs

envs::[String] -> IO [(String, String)]
envs keys = return . catMaybes =<< mapM' step keys
    where
        step::String -> IO (Maybe (String, String))
        step name = catch
            (return . Just . (name, )=<< getEnv name)
            (const $ return Nothing :: (Monad m) => SomeException -> m (Maybe a))

f = envs ["USER", "HOME"]

--
