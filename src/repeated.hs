
iter :: (a -> a) -> a -> [a]
iter f x = x : iter f (f x)

iter' f x = x : map f (iter' f x)

repeated :: (a -> a) -> Int -> a -> a
repeated f n x = iter f x !! n

printThenAdd :: (Show a, Num a) => IO a -> IO a
printThenAdd v = do { a <- v; print a; return (a + 1) }



f = repeated printThenAdd 5 (return 0)

g = take 10 $ iter' (+1) 1


h = take 10 ns
    where
        ns = 1 : map (+1) ns
