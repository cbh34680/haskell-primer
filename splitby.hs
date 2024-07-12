
splitBy::(Char -> Bool) -> String -> [String]
splitBy _ [] = []
splitBy f cs =
    let
        (hit, rest) = break f cs
        next = splitBy f (dropWhile f rest)
    in
        hit : next

f = splitBy (`elem` "&;") "abc&;def&ghi"
