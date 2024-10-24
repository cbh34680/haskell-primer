import Control.Exception
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.Bool


parseTest f cs =
    case (f cs) of
        Just _ -> print cs
        _ -> print ("error: " ++ cs)


anyChar [] = Nothing
anyChar (c:cs) = Just (c,cs)

satisfy f cs0 = do
    (a, cs1) <- anyChar cs0
    guard (f a)
    return (a, cs1)

digit = satisfy isDigit

test1 cs0 = do
    (a, cs1) <- anyChar cs0
    (b, cs2) <- anyChar cs1
    return ([a,b], cs2)


main = do
    parseTest anyChar "abcde"
    parseTest test1 "a"
    parseTest test1 "abcde"
    parseTest (satisfy (== 'A')) "abcde"
    parseTest (satisfy (== 'A')) ""
    parseTest digit "a"
    parseTest digit "1"



-- EOF
