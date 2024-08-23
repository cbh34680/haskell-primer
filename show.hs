
data Tree a = Leaf a | Branch (Tree a) (Tree a)

---------------------------

showTree :: Show a => Tree a -> String
showTree (Leaf x) = show x
showTree (Branch l r) = "<" ++ showTree l ++ "|" ++ showTree r ++ ">"

-- showsTree :: Show a => Tree a -> String -> String
-- showsTree (Leaf x) s = shows x s
-- showsTree (Branch l r) s = '<' : (showsTree l ('|' : (showsTree r ('>' : s))))

type ShowS' = String -> String

showsTree :: Show a => Tree a -> ShowS'
showsTree (Leaf x) = shows x
showsTree (Branch l r) = ('<' :) . showsTree l . ('|' :) . showsTree r . ('>' :)


dat = Branch (Leaf 1) (Leaf 2)

f = showTree dat
g = showsTree dat ""

---------------------------

readsTree :: (Read a) => ReadS (Tree a) -- String -> [(Tree a), String)]
readsTree ('<':s) = [(Branch l r, t') | (l, ('|':t)) <- readsTree s,
                                        (r, t')      <- readsTree t]
readsTree s = [(Leaf x, s') | (x, s') <- reads s]



i = map (\(t, s) -> showTree t ++ "; " ++ s) i'
    where
        i' :: [(Tree Int, String)]
        i' = readsTree $ showTree dat






-- EOF

