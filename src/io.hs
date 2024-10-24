{-# LANGUAGE UnboxedTuples #-}

import GHC.Base

f = do
    let a = IO $ \s -> (# s, 1 #)
    print =<< a

g = IO $ \s -> unIO (print 2) s

h = do
    d <- IO $ \s0 ->
        let
            (# s1, a #) = unIO (return 1) s0
            (# s2, b #) = unIO (return (a + 1)) s1
            (# s3, c #) = unIO (return (b + 1)) s2
        in
            (# s1, "[" ++ show c ++ "]" #)

    putStrLn d

i = do
    d <- let c = 3 in return ("[" ++ show c ++ "]")
    putStrLn d
    
