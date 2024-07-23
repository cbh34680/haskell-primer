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
