
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show


instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node lt rt) = Node (fmap f lt) (fmap f rt)


-- liftA2 (+) (Leaf 1) (Leaf 2)
-- liftA2 (+) (Node (Leaf 1) (Leaf 2)) (Leaf 2)

instance Applicative Tree where
    pure = Leaf

    (Leaf f) <*> t = fmap f t











-- EOF
