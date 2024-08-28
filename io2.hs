{-# LANGUAGE UnboxedTuples #-}

import GHC.Base


main = return' "hello" `bind` putStr `bind` print


return' a = IO $ \s -> (# s, a #)

bind ma f = IO $ \s0 ->
    let
        (# s1, a #) = unIO ma s0
        (# s2, b #) = unIO (f a) s1
    in
        (# s2, b #)
