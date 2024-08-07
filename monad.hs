import Control.Monad


sequence' :: Monad m => [m a] -> m [a]

{-
sequence' = foldr step (return [])
    where
        step x rl = do
            y <- x
            ys <- rl
            return (y:ys)
-}

sequence' = foldr (\x -> \rl -> x >>= \y -> rl >>= \ys -> return (y:ys)) (return [])


f = sequence' [Just 1]
g = sequence' [putChar 'a']


-- mapM' f = sequence' . map f

mapM' f [] = return []
mapM' f (x:xs) = f x >>= \y -> mapM' f xs >>= \ys -> return (y:ys)



liftM' :: Monad m => (a -> b) -> m a -> m b

liftM' f ma = ma >>= \a -> return (f a)


h = foldM (\l -> \x -> if x > 0 then Just (l+x) else Nothing) 0 $ [1,2,3,4,-1,5,6]



foldM' f z [] = return z
foldM' f z (x:xs) = f z x >>= \y -> foldM' f y xs







-- EOF
