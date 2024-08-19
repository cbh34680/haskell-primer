

sequence' [] = return []
sequence' (x:xs) = x >>= \y -> sequence' xs >>= \ys -> return (y:ys)

f = sequence' [(+1), (+2)] 1


