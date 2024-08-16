data Func r a = GenFunc { runFunc :: r -> a }

instance Functor (Func r) where
    fmap f g = GenFunc $ \r -> let a = (runFunc g) r in f a

instance Applicative (Func r) where
    pure x = GenFunc $ \r -> x
    m <*> n = GenFunc $ \r ->
        let f = runFunc m r
            x = runFunc n r
        in f x
                

instance Monad (Func r) where
    m >>= f = GenFunc $ \r -> let a = (runFunc m) r in (runFunc (f a)) r

fix :: Int -> Func a Int
fix n = GenFunc $ \r -> n

arg :: Func String String
arg = GenFunc $ \r -> r

step :: Func String Int
step = (fix 1) >> ((arg) >>= (\b -> (return (length b))))

f = runFunc step "abc"

g = runFunc (fmap (+1) (return 1)) 1

h = runFunc ((return (+1)) <*> (return 1)) 1

