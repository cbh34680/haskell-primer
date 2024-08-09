{-# LANGUAGE LambdaCase #-}

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Directory
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



-- dirTree :: FilePath -> IO Dir
dirTree path = do
    contents <- catch (listDirectory path) (const (return []) :: SomeException -> IO [a])
    files <- catMaybes <$> mapM lsFile contents

    return $ GenDir { dName=path, files=files, dirs=[] }




f = dirTree "."

