import qualified Control.Exception as E
import qualified System.IO.Error as E
import Control.Monad.IO.Class


main = do
    doSomething `E.catch` handleException


raise :: String -> IO a
raise msg = E.throwIO $ E.userError msg


doSomething :: IO ()
doSomething = do
    putStrLn "begin"
    raise "! Something happened"
    putStrLn "done."


handleException :: E.SomeException -> IO ()
handleException e = do
    putStrLn $ E.displayException e
