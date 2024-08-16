import System.Process
import System.IO
import GHC.IO.Exception
import Control.Concurrent
import Control.Exception

exitCode :: ProcessHandle -> IO Int
exitCode ph = do
    mec <- getProcessExitCode ph

    case mec of
        Just ec -> case ec of
                        ExitSuccess -> return 0
                        ExitFailure x -> return x
        Nothing -> threadDelay 10 >> exitCode ph


fmt :: String -> String
fmt = unlines . map ("\t" ++) . lines

putFmt = putStrLn . fmt


run = do
    (inp, out, err, ph) <- runInteractiveProcess "lsz" ["-lz"] (Just "/tmp") Nothing

    putStrLn "# exit"
    ec <- exitCode ph
    putFmt $ show ec

    putStrLn "# stdout"
    hGetContents out >>= putFmt

    putStrLn "# stderr"
    hGetContents err >>= putFmt


f = run `catch` handle
    where
        handle (SomeException e) = do
            putStrLn "# abort"
            putFmt $ displayException e



-- EOF
