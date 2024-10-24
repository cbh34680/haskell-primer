

tails :: Int -> [String] -> [String]

tails n = f []
    where
        f acc [] = acc
        f acc (cs:css) = f (take n (cs:acc)) css

main = do
    cs <- getContents
    putStrLn . unlines . tails 5 . lines $ cs
