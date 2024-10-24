-- {-# LANGUAGE LambdaCase #-}

import Control.Exception
import Control.Monad
import System.Directory
import System.Posix.Files
import System.Posix.Types


lessThan :: FileOffset -> FilePath -> IO Bool
lessThan threshold path = do
    s <- getFileStatus path
    return $ and [isRegularFile s, fileSize s < threshold]

smallFiles :: FileOffset -> FilePath -> IO [FilePath]
smallFiles threshold dir = do
    contents <- listDirectory dir
    filterM (lessThan threshold) contents


arithHandler :: ArithException -> IO [a]
arithHandler e = putStrLn ("Arith " ++ show e) >> return []
    
ioHandler :: IOException -> IO [a]
ioHandler e = putStrLn ("IO " ++ show e) >> return []

someHandler :: SomeException -> IO [a]
someHandler e = putStrLn ("SOME " ++ show e) >> return []

f dir = do
    smallFiles 100 dir
        `catch` arithHandler
        `catch` ioHandler
        `catch` someHandler

f' dir = do
    smallFiles 100 dir `catches` [
            Handler arithHandler,
            Handler ioHandler,
            Handler someHandler]


-- EOF
