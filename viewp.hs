{-# LANGUAGE ViewPatterns #-}

f = g [1..10]

g (length . filter odd -> n) = n

