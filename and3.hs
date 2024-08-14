

kvs = [ ["key1", "a01"], ["#key2", "c01"], ["key3", "d02"],
        ["key4", "b02"], ["key5", "a02", "x"], [], ["x"]]

cond1 = (== 2) . length                 -- 二つの要素がある
cond2 = ('#' /=) . head . (!! 0)        -- 1 つ目の先頭が # ではない
cond3 = (`elem` "abc") . head . (!! 1)  -- 2 つ目の先頭が "abc" のいずれか

and3 x y z = x && y && z

f = filter (\xs -> and ([cond1, cond2, cond3] <*> [xs])) kvs

g = filter g' kvs
    where
        g' = do
            a <- cond1
            b <- cond2
            c <- cond3
            return (and3 a b c)

h = filter (and3 <$> cond1 <*> cond2 <*> cond3) kvs

