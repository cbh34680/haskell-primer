import System.IO
import Control.Exception
import System.Random
import System.Directory


main = do
    (path, hndl) <- openTempFile "." "file.txt"
    putStrLn path

    r <- finally (writeRandom hndl) $ do
        hClose hndl
        removeFile path

    putStrLn $ show r


writeRandom :: Handle -> IO Int
writeRandom hndl = do
    r <- randomRIO (2020, 2100) :: IO Int
    hPutStrLn hndl $ show r
    return r
