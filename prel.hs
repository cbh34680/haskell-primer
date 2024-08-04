
import qualified Debug.Trace as T
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
    asTypeOf, undefined,
    seq, ($!)
    )

otherwise = True

undefined = error "\nProgram error: undefined\n"

not True = False
not _ = True

(.) f g x = f (g x)
($) f x = f x

const :: a -> b -> a
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

notElem x = not . elem x

(||) :: Bool -> Bool -> Bool
False || x = x
_     || _ = True

(&&) :: Bool -> Bool -> Bool
True && x = x
_    && _ = False


isSpace = (`elem` " \t\n\r")

isAlpha = (`elem` (['A'..'Z'] ++ ['a'..'z']))

isNumber = (`elem` ['0'..'9'])

isAlphaNum c
    | isAlpha c || isNumber c = True
    | otherwise = False

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

unlines :: [String] -> String

unlines = concat . map (++ "\n")

unwords [] = []
unwords (cs:css) = foldl (\acc xs -> acc ++ " " ++ xs) cs css


reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

and = foldr (&&) True
or = foldr (||) False

any f = or . (map f)
all f = and . (map f)



lookup _ [] = Nothing
lookup x ((k,w):rest)
    | x == k = Just w
    | otherwise = lookup x rest


sum = foldr (+) 0
product = foldr (*) 1

maximum [x] = x
maximum (x:xs) = max x $ maximum xs

minimum [x] = x
minimum (x:xs) = min x $ minimum xs

concatMap f = concat . (map f)


zip [] _  = []
zip _  [] = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

zip3 [] _  _  = []
zip3 _  [] _  = []
zip3 _  _  [] = []
zip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : zip3 xs ys zs


isUpper = (`elem` ['A'..'Z'])
isLower = (`elem` ['a'..'z'])
isDigit = (`elem` ['0'..'9'])


sequence [] = return []
sequence (x:xs) = do
    y <- x
    ys <- sequence xs
    return (y:ys)


mapM _ [] = return []
mapM f (x:xs) = do
    y <- f x
    ys <- mapM f xs
    return (y:ys)


{-
mapM_ _ [] = return ()
mapM_ f (x:xs) = do
    f x
    mapM_ f xs
-}
mapM_ f = foldr (\x acc -> f x >> acc) (return ())

{-
sequence_ [] = return ()
sequence_ (x:xs) = do
    x
    sequence_ xs
-}
sequence_ :: Monad m => [m a] -> m ()
sequence_ = foldr (\x acc -> x >> acc) (return ())


n ^ 0 = 1
n ^ m = n * (n ^ (m - 1))


until p f x
    | p x = x
    | otherwise = until p f (f x)

id x = x

fst (x, _) = x
snd (_, x) = x

curry f a b = f (a, b)
uncurry f (a,b) = f a b


asTypeOf :: a -> a -> a
asTypeOf = const


intersperse :: a -> [a] -> [a]

intersperse _ [] = []
intersperse _ [x] = [x]
intersperse x (y:ys) = y : x : intersperse x ys


transpose = undefined


group :: Eq a => [a] -> [[a]]
{-
group = f []
    where
        f ys [] = [ys]

        f [] (x:xs) = f [x] xs

        f ys@(y:_) (x:xs)
            | x == y = f (x:ys) xs
            | otherwise = ys : f [x] xs
-}

group [] = []
group [x] = [[x]]
group (x:xs) = (x:ys) : group zs
    where
        (ys, zs) = span (== x) xs



inits :: [a] -> [[a]]

{-
inits xs = reverse $ f xs
    where
        f [] = []
        f ys = init ys : f (init ys)
-}

inits [] = [[]]
inits (x:xs) = [[]] ++ map (x:) (inits xs)



tails [] = [[]]
tails xxs@(_:xs) = xxs : tails xs


isPrefixOf [] [] = True
isPrefixOf [] _  = True
isPrefixOf _  [] = False

isPrefixOf (x:xs) (y:ys)
    | x == y = isPrefixOf xs ys
    | otherwise = False



isSuffixOf [] [] = True
isSuffixOf [] _  = False
isSuffixOf _  [] = False

isSuffixOf xxs@(x:xs) yys@(y:ys)
    | lx >  ly = False
    | lx <  ly = isSuffixOf xxs ys
    | (lx == ly) && (x == y) = isSuffixOf xs ys
    | otherwise = False
    where
        lx = length xxs
        ly = length yys

-- isInfixOf xs = or . map (xs `isPrefixOf`) . tails
isInfixOf xs = any (xs `isPrefixOf`) . tails


find _ [] = Nothing
find p (x:xs)
    | p x = Just x
    | otherwise = find p xs



{-
partition _ [] = ([], [])
partition p (x:xs)
    | p x = (x:ys, zs)
    | otherwise = (ys, x:zs)
    where
        (ys, zs) = partition p xs
-}

partition p = foldr f ([], [])
    where
        f x (ls, rs)
            | p x = (x:ls, rs)
            | otherwise = (ls, x:rs)


elemIndex x xs = lookup x $ zip xs [0..]

{-
lookups' _ [] = []
lookups' x ((k,w):rest)
    | x == k = w : lookups' x rest
    | otherwise = lookups' x rest

elemIndices x xs = lookups' x $ zip xs [0..]



elemIndices x xs = [ b | (a,b) <- zip xs [0..], x == a ]
-}


elemIndices x xs = findIndices (== x) xs

findIndex p xs = case findIndices p xs of
    [] -> Nothing
    ys -> Just ys

findIndices p xs = [ b | (a,b) <- zip xs [0..], p a ]


zipWith _ [] _  = []
zipWith _ _  [] = []
zipWith p (x:xs) (y:ys) = p x y : zipWith p xs ys



nub :: Eq a => [a] -> [a]
{-
nub = reverse . foldl (\acc x -> if x `elem` acc then acc else x:acc) []
-}

nub ys = f [] ys
    where
        f _ [] = []
        f acc (x:xs)
            | x `elem` acc = f acc xs
            | otherwise = x : f (x:acc) xs

safe_head' [] = []
safe_head' xs = head xs

safe_tail' [] = []
safe_tail' xs = tail xs

delete x xs = ys ++ safe_tail' zs
    where
        (ys, zs) = break (== x) xs


[] \\ _  = []
xs \\ [] = xs
(x:xs) \\ ys
    | x `elem` ys = xs \\ ys
    | otherwise = x: xs \\ ys



union xs [] = xs
union xs (y:ys)
    | y `elem` xs = union xs ys
    | otherwise = union (xs ++ [y]) ys



insert :: Ord a => a -> [a] -> [a]
insert _ [] = []
insert x (y:ys)
    | y < x = y : insert x ys
    | otherwise = x : y : ys



groupBy _ [] = []
groupBy p (x:xs) = (x:ys) : groupBy p zs
    where
        (ys, zs) = span (p x) xs


grouping' _ [] = []

grouping' p xs = ys : grouping' p zs
    where
        (ys, zs) = f xs (tail xs)

        f ys [] = (ys, [])

        f (y:ys) (z:zs)
            | p y z = ((y:ys'), zs')
            | otherwise = ([y], ys)
            where
                (ys', zs') = f ys zs



{-
-- [1,2] %>> [[3,4], [5,6]] --->> [[1,3,4], [2,5,6]]

(%>>) :: [a] -> [[a]] -> [[a]]
ls %>> rss = map (\(x,xs) -> x:xs) $ zip ls rss
f xss@(xs:_) = foldr (%>>) (replicate (length xs) []) xss
-}


intersect xs ys = [ x | x <- xs, any (== x) ys ]


transpose' :: [[a]] -> [[a]]
transpose' = foldr step []
    where
        step :: [a] -> [[a]] -> [[a]]
        step [] [] = []
        step [] (ys:yss) = ys : step [] yss
        step (x:xs) [] = [x] : step xs []
        step (x:xs) (ys:yss) = (x:ys) : step xs yss


f = transpose' [[1,2],[3,4],[5],[6,7,8]]




















-- EOF
