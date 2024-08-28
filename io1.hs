{-# LANGUAGE UnboxedTuples #-}

import GHC.Base
import System.Random

shuffle [] = IO $ \s -> (# s, [] #)

shuffle xs = do
    IO $ \s0 ->
        let
            (# s1, n #) = unIO (getStdRandom $ randomR (0, length xs - 1) :: IO Int) s0
            (# s2, xs' #) = unIO (shuffle $ take n xs ++ drop (n + 1) xs) s1
        in
            (# s2, (xs !! n) : xs' #)

main = do
    IO $ \s0 ->
        let
            (# s1, xs #) = unIO (shuffle [1..9]) s0
            (# s2, a  #) = unIO (print xs) s1
        in
            (# s2, a #)

