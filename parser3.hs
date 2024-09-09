import Control.Exception
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.Bool


parseTest f cs =
    case (evalStateT f cs) of
        Just _ -> print cs
        _ -> print ("error: " ++ cs)


anyChar :: StateT String Maybe Char
anyChar = StateT $ f
    where
        f [] = Nothing
        f (c:cs) = Just (c, cs)

satisfy f = do
    a <- anyChar
    guard (f a)
    return a

digit = satisfy isDigit

test1 = do
    a <- anyChar
    b <- anyChar

    return [a,b]


main = do
    parseTest anyChar "abcde"
    parseTest anyChar ""
    parseTest test1 "a"
    parseTest test1 "abcde"
    parseTest (satisfy (== 'A')) "abcde"
    parseTest (satisfy (== 'A')) ""
    parseTest digit "a"
    parseTest digit "1"



-- EOF
