import Data.Char

wordsCount = outWord

outWord [] = 0
outWord (c:cs)
    | isAlphaNum c = 1 + inWord  cs
    | otherwise    =     outWord cs

inWord [] = 0
inWord (c:cs)
    | isAlphaNum c =     inWord  cs
    | otherwise    =     outWord cs



wordsCount' = outWord'

wordScan _ [] = 0
wordScan f (c:cs)
    | isAlphaNum c = f (inWord' cs)
    | otherwise    = outWord' cs


inWord'  cs = wordScan id   cs
outWord' cs = wordScan (+1) cs








































-- EOF
