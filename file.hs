import Control.Exception
import System.IO


closeFile :: Handle -> IO ()
closeFile h = do
    putStrLn "closeFile"
    hClose h


main = do
    bracket (openFile "/etc/passwd" ReadMode) closeFile $
        \hdl -> do
            hGetLine hdl >>= putStrLn

    putStrLn "done."
