import Data.Monoid

f = plus1 `mappend` plus2 $ 1
    where
        plus1 = Sum . (+1)
        plus2 = Sum . (+2)

f' = (+) <$> (+1) <*> (+2) $ 1

{-
newtype Endo a = Endo { appEndo :: a -> a }

instance Monoid (Endo a) where
        mempty = Endo id
        Endo f `mappend` Endo g = Endo (f . g)



class Foldable t where
        fold :: Monoid m => t m -> m
        fold = foldMap id

        foldMap :: Monoid m => (a -> m) -> t a -> m
        foldMap f = foldr (mappend . f) mempty
...
-}


fldr f z xs = appEndo (foldMap (Endo . f) xs) z

g = fldr (+) 0 [1,2,3]

g' = appEndo (Endo (+1) `mappend` Endo (+2) `mappend` Endo (+3)) 0

g'' = (+1) . (+2) . (+3) $ 0


-- EOF
