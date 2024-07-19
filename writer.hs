-- hugs98

import Data.Monoid
import Control.Applicative


newtype MyWriter w a = GenMyWriter { runMyWriter :: (a, w) } deriving Show

instance (Monoid w) => Functor (MyWriter w) where
    fmap f (GenMyWriter (a, w)) = GenMyWriter (f a, w)

instance (Monoid w) => Applicative (MyWriter w) where
    pure x = GenMyWriter (x, mempty)
    liftA2 f (GenMyWriter (x, xw)) (GenMyWriter (y, yw)) = GenMyWriter ((f x y), (mappend xw yw))

instance (Monoid w) => Monad (MyWriter w) where
    return = pure
    (GenMyWriter (x, xw)) >>= f =
        let (GenMyWriter (y, yw)) = f x in GenMyWriter (y, mappend xw yw)

addNum :: (Show a) => a -> MyWriter [String] a
addNum x = GenMyWriter (x, ["add " ++ show x])


f1 :: MyWriter [String] Int
f1 = GenMyWriter (1, ["with 1"])

f2 :: MyWriter [String] Int
f2 = GenMyWriter (2, ["with 2"])


f' :: MyWriter [String] Int
f' = do
    a <- addNum 1
    b <- addNum 2
    c <- addNum 3
    return (a + b + c)

f = runMyWriter f'


