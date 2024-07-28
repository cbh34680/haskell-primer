
import Data.List ()

import Prelude hiding (
    map, (++), concat, filter,
    head, last, tail, init, null, length, (!!),
    foldl, foldl1, scanl, scanl1, foldr, foldr1, scanr, scanr1,
    iterate, repeat, replicate, cycle,
    take, drop, splitAt, takeWhile, dropWhile, span, break,
    lines, words, unlines, unwords, reverse, and, or,
    any, all, elem, notElem, lookup,
    sum, product, maximum, minimum, concatMap,
    zip, zip3, zipWith, zipWith3, unzip, unzip3,

    isSpace, isUpper, isLower,
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum,
    readLitChar, showLitChar, lexLitChar,

    mapM, mapM_, sequence, sequence_, (=<<),
    maybe, either,
    (&&), (||), not, otherwise,
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,
    fst, snd, curry, uncurry, id, const, (.), flip, ($), until,
    asTypeOf, error, undefined,
    seq, ($!)
    )

otherwise = True

not True = False
not _ = True

(.) f g x = f (g x)
($) f x = f x

const a b = a

map _ [] = []
map f (x:xs) = f x : map f xs

[] ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

concat [] = []
concat (xs:xss) = xs ++ concat xss

filter _ [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

head (x:_) = x
tail (_:xs) = xs

init [x] = []
init (x:xs) = x: init xs

last [x] = x
last (_:xs) = last xs

null [] = True
null _ = False

-- length [] = 0
-- length (_:xs) = 1 + length (xs)

length = foldl (\acc _ -> acc + 1) 0

(x:xs) !! n
    | n == 0 = x
    | otherwise = xs !! (n - 1)


foldr f acc [] = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

foldl f acc [] = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr1 f (x:xs) = foldr f x xs
foldl1 f (x:xs) = foldl f x xs

scanr f acc [] = [acc]
scanr f acc (x:xs) = f x y : ys
    where
        ys@(y:_) = scanr f acc xs

scanl f acc [] = [acc]
scanl f acc (x:xs) = acc : scanl f (f acc x) xs

scanr1 f [x] = [x]
scanr1 f (x:xs) = f x y : ys
    where
        ys@(y:_) = scanr1 f xs

scanl1 f (x:xs) = scanl f x xs

flip f a b = f b a

subtract = flip (-)

{-
even x = (x `mod` 2 == 0)
odd = not . even
-}

even 0 = True
even n = odd (n - 1)

odd 0 = False
odd n = even (n - 1)


iterate f x = x : iterate f (f x)

repeat x = xs where xs = x : xs

num' = f 0
    where
        f x = x : f (x + 1)

elem :: Eq a => a -> [a] -> Bool
_ `elem` [] = False
x `elem` (y:ys)
    | x == y = True
    | otherwise = x `elem` ys

isSpace :: Char -> Bool
isSpace c = c `elem` " \t\n\r"

replicate n x = [ x | _ <- [1 .. n] ]

cycle xs = xs ++ cycle xs

take 0 _ = []
take n (x:xs) = x : take (n-1) xs

drop 0 xs = xs
drop n (x:xs) = drop (n-1) xs

splitAt 0 xs = ([], xs)
splitAt n (x:xs) = (x:ys, zs)
    where
        (ys, zs) = splitAt (n-1) xs

takeWhile _ [] = []
takeWhile p (x:xs)
    | p x = x : takeWhile p xs
    | otherwise = []

dropWhile _ [] = []
dropWhile p xs'@(x:xs)
    | p x = dropWhile p xs
    | otherwise = xs'

break _ [] = ([], [])
break p xs'@(x:xs)
    | p x = ([], xs')
    | otherwise = (x:ys, zs)
        where
            (ys, zs) = break p xs

span p xs = break (not . p) xs

{-
lines [] = []

lines cs = 
    let
        (cs', rest) = break (== '\n') $ dropWhile (== '\n') cs
        in
            if cs' == "" then [] else cs' : (lines rest)
lines cs =
    case cs' of
        "" -> []
        _  -> cs' : lines rest
    where
        (cs', rest) = break (== '\n') $ dropWhile (== '\n') cs

words cs =
    case cs' of
        "" -> []
        _  -> cs' : words rest
    where
        (cs', rest) = break isSpace $ dropWhile isSpace cs
-}

splitBy' :: (Char -> Bool) -> String -> [String]
splitBy' p cs =
    case cs' of
        "" -> []
        _  -> cs' : splitBy' p rest
    where
        (cs', rest) = break p $ dropWhile p cs

lines = splitBy' (== '\n')
words = splitBy' isSpace








-- EOF
