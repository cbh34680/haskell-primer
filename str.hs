import qualified Data.Char as C

trimLeft::String -> String
trimLeft = dropWhile C.isSpace

trimRight'::String -> String
trimRight' = concat . f ""
    where
        f::String -> String -> [String]
        f ys [] = []
        f ys (x:xs)
            | C.isSpace x = f (x : ys) xs
            | otherwise = (reverse (x : ys)) : f "" xs

trimRight::String -> String
trimRight = foldr f []
    where
        f x [] | C.isSpace x = []
        f x ys = x : ys

substring::Int -> Int -> String -> String
substring s l = take l . drop s

slice::Int -> Int -> [a] -> [a]
slice s e = take (e - s) . drop s
