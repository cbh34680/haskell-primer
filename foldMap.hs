import Data.Monoid


fldMap :: Monoid m => (a -> m) -> [a] -> m

--fldMap f = foldr (\x r -> f x `mappend` r) mempty
--fldMap f = foldr (\x r -> mappend (f x) r) mempty
fldMap f = foldr (mappend . f) mempty

f = fldMap Sum [1..10]

add2 :: Int -> Int
add2 = add1 1
    where
        add1 :: Int -> Int -> Int
        add1 = (+) . (+1)


fldr f z xs = appEndo (fldMap (Endo . f) xs) z

g = fldr (+) 0 [1..10]
