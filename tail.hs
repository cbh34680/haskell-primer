
main = do
    cs <- getContents
    putStr . unlines . reverse . lastLines 10 $ lines cs
    where
        lastLines n = foldl (\acc s -> take n (s : acc)) []

