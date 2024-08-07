import qualified Data.List as DL
import Data.Function ((&))
import qualified System.Random as SR

{-
https://wiki.haskell.org/99_questions
-}

myLast :: [a] -> a
myLast = foldl1 (flip const)

myButLast :: [a] -> a
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

elementAt :: [a] -> Int -> Maybe a
-- elementAt xs i = xs !! (i - 1)
elementAt xs i = lookup i $ zip [1..] xs

myLength :: [a] -> Int
myLength = sum . map (const 1)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

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
        m = if n >= 0 then n else l- (abs n)


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
{-
rnd_select xs n = do
    ys <- sequence . replicate n $ randomInt (0, length xs - 1)
    return $ map (xs !!) ys

---
ghci> rnd_select ['a'..'z'] 3
"dfj"

-}
-- rnd_select xs n = sequence . replicate n $ randomInt (0, length xs - 1) >>= return . (xs !!)
rnd_select xs n = sequence . replicate n $ randomInt (0, length xs - 1) >>= \ys -> return ( map (xs !!) ys )
















-- EOF
