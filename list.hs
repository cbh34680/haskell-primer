nss = [[1,2], [3,4], [5,6]]

f = foldr f' (return []) nss
    where
        f' xs acc = xs >>= \a -> acc >>= \b -> return (a:b)

g = foldr g' (return []) nss
    where
        g' xs acc = xs >>= g''
            where
                g'' = \a -> acc >>= \b -> return (a:b)

h = foldr h' (return []) nss
    where
        h' xs acc = concatMap ( \a -> concatMap (\b -> return (a:b)) acc ) xs


i = do
    a <- [1,2]
    b <- [3,4]
    c <- [5,6]
    return [a,b,c]

j = [1,2] >>= \a -> [3,4] >>= \b -> [5,6] >>= \c -> return [a,b,c]

k = concatMap (\a -> concatMap (\b -> concatMap (\c -> return [a,b,c]) [5,6]) [3,4]) [1,2]
