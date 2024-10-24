
sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr mcons (return [])

mcons :: Monad m => m t -> m [t] -> m [t]
mcons p q = do
  x <- p
  y <- q
  return (x:y)


f = sequence' [putStrLn "a", putStrLn "b"]
g = sequence' [[1,2], [3,4]]
h = sequence' [Just 1, Just 2]
h' = sequence' [Just 1, Nothing, Just 2]
