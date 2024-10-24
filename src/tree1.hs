
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show


instance Functor Tree where
    fmap f Leaf = Leaf
    fmap f (Node lt x rt) = Node (fmap f lt) (f x) (fmap f rt)

-- fmap (+1) (Node Leaf 1 Leaf)


instance Applicative Tree where
    pure x = Node Leaf x Leaf
    Leaf <*> _ = Leaf
    _ <*> Leaf = Leaf
    (Node lt f rt) <*> (Node lt' x rt') = Node (lt <*> lt') (f x) (rt <*> rt')


-- (+) <$> Node Leaf 1 Leaf <*> pure 2

-- pure id <*> x = x
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- pure f <*> pure x = pure (f x)
-- u <*> pure x = pure ($ x) <*> u
-- pure f <*> x = fmap f x

instance Monad Tree where
    Leaf >>= _ = Leaf
    (Node lt x rt) >>= f = f x

-- 

left :: Num a => (a -> a) -> a -> Tree a

left f x = Node (return . f $ x) x Leaf




-- EOF
