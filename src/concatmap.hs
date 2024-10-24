
xs = [1..3]
ys = [1..3]


f = map (\x -> map (\y -> (x,y)) xs) ys
f' = concat f


g = [(x,y) | x<-xs, y<-ys]


h = do
    x <- xs
    y <- ys
    return (x,y)

h' = xs >>= \x -> ys >>= \y -> return (x,y)

h'' = (,) <$> xs <*> ys




