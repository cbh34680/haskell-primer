{-# LANGUAGE MagicHash, UnboxedTuples #-}

import GHC.Base
import GHC.ST

import Control.Monad.ST
import Data.STRef


unST (ST f) = f

f = do
    let f = unST $ (return 1 :: ST s Int)
        (# _, x #) = f realWorld#

    print x


