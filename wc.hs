import Data.Char

wordsCount = outWord

outWord [] = 0
outWord (x:xs)
    | isSpace x = outWord xs
    | otherwise = 1 + inWord xs

inWord [] = 0
inWord (x:xs)
    | isSpace x = outWord xs
    | otherwise = inWord xs


linesCount = outLine

outLine [] = 0
outLine (x:xs)
    | x == '\n' = outLine xs
    | otherwise = 1 + inLine xs

inLine [] = 0
inLine (x:xs)
    | x == '\n' = outLine xs
    | otherwise = inLine xs









































-- EOF
