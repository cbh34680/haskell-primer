import qualified Data.List as DL
import Data.Function ((&))
import qualified System.Random as SR
import qualified Control.Monad as CM

{-
https://wiki.haskell.org/99_questions

hlint -i "Use camelCase" p99.hs
-}

myLast :: [a] -> a
--myLast = foldl1 (flip const)
--myLast = foldl1 (const id)
myLast = foldl1 (\_ x -> x)

myButLast :: [a] -> a
--myButLast (x:_:[]) = x
myButLast [x, _] = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> Maybe a
-- elementAt xs i = xs !! (i - 1)
elementAt xs i = lookup i $ zip [1..] xs

myLength :: [a] -> Int
myLength = sum . map (const 1)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
--isPalindrome xs = xs == (reverse xs)
isPalindrome xs = xs == reverse xs

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List xs) = concatMap flatten xs

compress :: Eq a => [a] -> [a]
compress = map head . DL.group

pack :: Eq a => [a] -> [[a]]
pack = DL.group

encode :: Eq a => [a] -> [(Int,a)]
encode xs = zip (map length gr) (map head gr)
    where
        gr = DL.group xs

data Combination a = Single a | Multiple Int a deriving Show

toCombination 1 c = Single c
toCombination n c = Multiple n c


-- P11

encodeModified :: Eq a => [a] -> [Combination a]
encodeModified = map (uncurry toCombination)  . encode

decodeModified :: [Combination a] -> [a]
decodeModified = concatMap f
    where
        f (Single c) = [c]
        f (Multiple n c) = replicate n c

encodeDirect :: Eq a => [a] -> [Combination a]
encodeDirect (x:xs) = f 1 x xs
    where
        f _ _ [] = []
        f n y (z:zs)
            | y == z = f (n + 1) y zs
            | otherwise = toCombination n y : f 1 z zs

dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])

repli :: [a] -> Int -> [a]
repli xs n = concat $ replicate n xs

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [ x | (i, x) <- zip (cycle [0 .. (n-1)]) xs, i /= (n-1)]

split :: [a] -> Int -> ([a], [a])
split = flip DL.splitAt

slice :: [a] -> Int -> Int -> [a]
--slice xs a b = let c = a - 1 in take (b - c) $ drop c xs
slice xs a b = let c = a - 1 in drop c xs & take (b - c)

rotate :: [a] -> Int -> [a]
rotate xs n = drop m ys & take l
    where
        ys = cycle xs
        l = length xs
        m = if n >= 0 then n else l - abs n
--        m = if n >= 0 then n else l - (abs n)


removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "no more"
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs)
    | n <= 0 = error "bad index"
    | otherwise = (y, x:ys)
        where
            (y, ys) = removeAt (n - 1) xs


-- P21

insertAt :: a -> [a] -> Int -> [a]
insertAt x xs@(y:ys) n
    | n <= 0 = error "bad index"
    | n == 1 = x:xs
    | otherwise = y : insertAt x ys (n - 1)


range :: Int -> Int -> [Int]
range s e = [s .. e]

randomInt :: (Int, Int) -> IO Int
randomInt = SR.randomRIO

rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = error "no data"

rnd_select xs n = f >>= g
    where
        l = length xs - 1
        g = return . map (xs !!)
        f = CM.replicateM n $ randomInt (0, l)
--        f = sequence . replicate n $ randomInt (0, l)

{-
another

[1]
rnd_select xs n = do
    ys <- sequence . replicate n $ randomInt (0, length xs - 1)
    return $ map (xs !!) ys

[2]
rnd_select xs n = f g
    where
        f = sequence . replicate n
        g = randomInt (0, length xs - 1) >>= return . (xs !!)

---
ghci> rnd_select ['a'..'z'] 3
"dfj"
-}


uniqRandom :: Int -> [Int] -> IO Int
uniqRandom mx xs = do
    x <- randomInt (0, mx)
    if x `elem` xs then uniqRandom mx xs else return x


diff_select :: Int -> Int -> IO [Int]
diff_select 0 _ = return []
diff_select n mx = do
    xs <- diff_select (n - 1) mx
    x <- uniqRandom mx xs

    return (x:xs)


snds :: [(a,b)] -> [b]
snds = foldr (\(_,x) acc -> x:acc) []

rnd_permu :: Ord a => [a] -> IO [a]
rnd_permu xs = foldr f (return []) xs >>= \ys -> return (g ys xs)
    where
        f x acc = do
            ls <- acc
            x <- uniqRandom (length xs - 1) ls
            return (x:ls)

        g ys xs = zip ys xs & DL.sort & snds


-- TODO: p26
-- TODO: p27


lsort :: [[a]] -> [[a]]
lsort = DL.sortBy (\a b -> compare (length a) (length b))


-- TODO: p28.b
-- TODO: p31 - 60

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving Show

tree4 = Branch 1 (Branch 2 Empty (Branch 4 Empty Empty))
                 (Branch 2 Empty Empty)

countLeaves :: Tree a -> Int
countLeaves Empty = 0
countLeaves (Branch _ Empty Empty) = 1
countLeaves (Branch _ l r) = countLeaves l + countLeaves r

-- TODO: p61.A - 99


-- EOF
