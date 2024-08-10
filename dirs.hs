{-# LANGUAGE LambdaCase #-}

import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Typeable
import System.Directory
import System.FilePath.Posix
import System.Posix.Files
import System.Posix.Types
import Data.Functor


data File = GenFile { fName::FilePath, fSize::FileOffset } deriving Show
data Dir = GenDir { dName::FilePath, files::[File], dirs::[Dir] } deriving Show


lsFile :: FilePath -> IO (Maybe File)
lsFile path = do
    s <- getFileStatus path

    if isRegularFile s then 
        return (Just (GenFile {fName=path, fSize=fileSize s}))
        else return Nothing


isDir :: FilePath -> IO Bool
isDir path = do
    s <- getFileStatus path
    return $ isDirectory s


dirTree :: FilePath -> IO Dir
dirTree arg = do
    let path = dropTrailingPathSeparator arg

    contents <- listDirectory path
    files <- catMaybes <$> mapM (lsFile . (path </>)) contents

    dirNames <- filterM (isDir . (path </>)) contents
    dirs <- mapM (dirTree . (path </>)) dirNames

    return $ GenDir { dName=path, files=files, dirs=dirs }


printTree :: String -> IO ()
--printTree path = f `catch` (const (return ()) :: SomeException -> IO ())
printTree path = f `catch` h
    where
        f = do
            tree <- dirTree path
            print tree

        h :: SomeException -> IO ()
        h (SomeException e) = do
            putStrLn (show (typeOf e))
            putStrLn (displayException e)
    

f = printTree "/home/ubuntu/a"


-- EOF
