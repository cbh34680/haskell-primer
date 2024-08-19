
map' _ [] = []
map' f (x:xs) = f x : map' f xs

num = 1 : map' (+1) num

f = take 10 $ num


