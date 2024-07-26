
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


flip f a b = f b a

subtract = flip (-)

even x = (x `div` 2 == 0)
odd x = not . even

