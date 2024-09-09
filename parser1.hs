import Control.Exception
import Control.Monad.State
import Data.Char


parseTest f cs = print (f cs) `catch` \(SomeException e) -> print e


anyChar (c:cs) = (c,cs)

satisfy f (c:cs) | f c = (c,cs)

digit = satisfy isDigit

test1 cs0 =
    let
        (a, cs1) = anyChar cs0
        (b, cs2) = anyChar cs1
    in
        ([a,b], cs2)


main = do
    parseTest anyChar "abcde"
    parseTest test1 "abc"
    parseTest test1 "abcde"
    parseTest (satisfy (== 'A')) "abcde"
    parseTest (satisfy (== 'A')) ""
    parseTest digit "a"
    parseTest digit "1"



-- EOF
