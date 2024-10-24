import Control.Applicative

data Fn a b = GenFn { unFn :: ((->) a b) }

instance Functor (Fn a) where
    fmap g (GenFn f) = GenFn $ g . f

instance Applicative (Fn a) where
    pure x = GenFn $ \_ -> x
    (GenFn f) <*> (GenFn g) = GenFn $ \x -> f x (g x)


f' = unFn $ (+) <$> GenFn (+1) <*> GenFn (+2)
f = f' 1
