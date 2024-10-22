{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import Text.Show.Pretty (ppShow)
import Data.Foldable (for_)
import Data.Traversable (for)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Control.Exception
import Data.Time (showGregorian)

main :: IO () 
main = do
    let ci = defaultConnectInfo {ciDatabase="hsdb",
                ciUser="hsdb_user", ciPassword="hsdb_pass"}

    bracket (connect ci) close dbmain


dbmain conn = do
    (defs, is) <- query_ conn "SELECT * FROM dept_manager"
    rows <- Streams.toList is

    print $ map columnName defs

    for_ rows $ \ [MySQLInt32 emp_no, MySQLText dept_no_t,
                    MySQLDate from_date_d, MySQLDate to_date_d] -> do

        let dept_no = T.unpack dept_no_t
        let from_date = showGregorian from_date_d
        let to_date = showGregorian to_date_d

        print (emp_no, dept_no, from_date, to_date)


-- EOF
