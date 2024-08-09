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


smallFiles :: FileOffset -> FilePath -> IO (Either String [FilePath])
smallFiles threshold dir = do
    ei <- try (listDirectory dir)

    case ei of
        Left (e::SomeException) -> return $ Left (displayException e)

        Right contents -> do
            paths <- filterM (lessThan threshold) contents
            return $ Right paths


smallFiles' :: FileOffset -> FilePath -> IO [FilePath]
smallFiles' threshold dir = do
    contents <- listDirectory dir `catch` (const (return []) :: SomeException -> IO [a])
    filterM (lessThan threshold) contents


















-- EOF
