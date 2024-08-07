import Data.Monoid


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node l r) = Node (fmap f l) (fmap f r)


foldTree :: Monoid a => Tree a -> a

foldTree (Leaf x) = x
-- foldTree (Node l r) = foldTree l `mappend` foldTree r
foldTree (Node l r) = foldTree l <> foldTree r

sumLeaf = Leaf . Sum

{-
ghci> foldTree $ Node (sumLeaf 1) (Node (sumLeaf 2) (sumLeaf 3))
Sum {getSum = 6}
it :: Num a => Sum a
-}


instance Foldable Tree where
    foldMap f (Leaf x) = f x
    foldMap f (Node l r) = foldMap f l <> foldMap f r

{-
ghci> foldMap Sum $ Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
Sum {getSum = 6}
it :: Num a => Sum a
ghci> foldr (+) 0 $ Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
6
it :: Num b => b
-}


dec :: Int -> Maybe Int
dec x = if x > 0 then Just (x - 1) else Nothing


instance Traversable Tree where
    traverse f (Leaf x) = pure Leaf <*> f x
    traverse f (Node l r) = pure Node <*> traverse f l <*> traverse f r

{-
ghci> traverse dec $ Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
Just (Node (Leaf 0) (Node (Leaf 1) (Leaf 2)))
it :: Maybe (Tree Int)
ghci> traverse dec $ Node (Leaf 1) (Node (Leaf 0) (Leaf 3))
Nothing
it :: Maybe (Tree Int)
-}


filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p = foldMap (\x -> if p x then [x] else [])

{-
ghci> filterF (/= 0) $ Node (Leaf 1) (Node (Leaf 0) (Leaf 3))
[1,3]
it :: (Eq a, Num a) => [a]
-}



-- EOF
