import Data.Char
import Control.Applicative

type Parse a = String -> [(a, String)]


succeed :: a -> Parse a
succeed v inp = [(v, inp)]


check :: (Char -> Bool) -> Parse Char
check p (c:cs)
    | p c = succeed c cs
check _ xs = []


-- char 'a'
char :: Char -> Parse Char
char = check . (==)


-- (char 'a' `alt` char 'b') "abc"
alt :: Parse a -> Parse a -> Parse a
(p1 `alt` p2) inp = p1 inp ++ p2 inp
-- (p1 `alt` p2) inp = p1 inp <|> p2 inp


-- (check isDigit `build` digitToInt) "1ab"

build :: Parse a -> (a -> b) -> Parse b
build p f = \inp -> [ (f x, y) | (x, y) <- p inp ]

{-
    check isDigit "1ab" --> [('1',"ab")]
    (x, y) <- ('1',"ab")

        x='1'
        y="ab"

    (digitToInt '1', "ab")
    [(1, "ab")]
-}


-- (char 'a' >@> char 'b') "abcde" --> [(('a', 'b'), "cde")]

(>@>) :: Parse a -> Parse b -> Parse (a, b)
(p1 >@> p2) inp = [ ((x,y), r2) | (x, r1) <- p1 inp, (y, r2) <- p2 r1]


--  (char 'a' >@ char 'b') "abcde" --> [('a', "cde")]

(>@) :: Parse a -> Parse b -> Parse a
p1 >@ p2 = (p1 >@> p2) `build` fst


--  (char 'a' @> char 'b') "abcde" --> [('b', "cde")]

(@>) :: Parse a -> Parse b -> Parse b
p1 @> p2 = (p1 >@> p2) `build` snd


-- (char 'a' >@ eof) "a"
eof :: Parse ()

eof "" = [((), [])]
eof _ = []





-- EOF
