
{-# LANGUAGE RankNTypes #-}

applyTuple :: (forall a. Enum a => a -> a) -> (Bool, Char)
applyTuple f = (f False, f 'a')

-- applyTuple' :: (a -> a) -> (Bool, Char)
-- applyTuple' f = (f False, f 'a')


