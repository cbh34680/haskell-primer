{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Text.Show.Pretty (ppShow)
import Data.Foldable (for_)
import Data.Traversable (for)
import qualified Data.ByteString.Char8 as BS


main :: IO () 
main = do
    conn <- connect
        defaultConnectInfo {ciUser = "hsdb_user", ciPassword = "hsdb_pass", ciDatabase = "hsdb"}
    (defs, is) <- query_ conn "SELECT * FROM dept_manager"


    rows <- Streams.toList is
    --putStrLn . ppShow $ rows


    let arr = map (zipWith (\def col -> (BS.unpack $ columnName def) ++ "=" ++ show col) defs) $ take 3 rows
    putStrLn $ ppShow arr

