{-# LANGUAGE UnboxedTuples #-}

import GHC.Base
import GHC.ST

import Control.Monad.ST
import Data.STRef


unST (ST f) = f

f = IO $ \s0 ->
    let f = unST $ (return 1 :: ST s Int)
        (# s1, x #) = f s0

        g = unIO $ (print x)
        (# s2, y #) = g s1

    in
        (# s2, y #)


