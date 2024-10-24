

sequence' [] = return []
sequence' (x:xs) = x >>= \y -> sequence' xs >>= \ys -> return (y:ys)

f = sequence' [(+1), (+2)] 1


sequence'' [] = return []

sequence'' (x:xs) = do
    y <- x
    ys <- sequence'' xs
    return (y:ys)

g = sequence'' [(+1), (+2)] 1
