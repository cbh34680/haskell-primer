import Data.List
import Debug.Trace

import qualified Data.Char as C

trimLeft::String -> String
trimLeft = dropWhile C.isSpace

data Color = Red | Green | Blue deriving (Show)

debug = flip trace

--
instance Read Color where
    readsPrec _ value = tryParse [("Red", Red), ("Green", Green), ("Blue", Blue)]
        where
            tryParse [] = []

            tryParse ((name, color):xs) =
                if name == (take (length name) $ trimLeft value) then
                    [(color, drop (length name) . dropWhile C.isSpace $ value)] `debug` mconcat ["OK: ", name, "==", value]
                else
                    tryParse xs `debug` mconcat ["NG: ", name, "==", value]

--
