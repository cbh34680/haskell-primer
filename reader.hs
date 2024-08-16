-- hugs

data Func r a = GenFunc { runFunc :: r -> a }

instance Monad (Func x) where
    return x = GenFunc $ \r -> x
    m >>= f = GenFunc $ \r -> let a = (runFunc m) r in (runFunc (f a)) r

fix :: Int -> Func a Int
fix n = GenFunc $ \r -> n

arg :: Func String String
arg = GenFunc $ \r -> r

step :: Func String Int
step = (fix 1) >> ((arg) >>= (\b -> (return (length b))))

f = runFunc step "abc"








