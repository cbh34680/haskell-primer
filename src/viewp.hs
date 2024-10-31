{-# LANGUAGE ViewPatterns #-}

f = g [1..10]

g (length . filter odd -> n) = n

--

lookupDefault :: Eq a => a -> b -> [(a,b)] -> b
lookupDefault k _   (lookup k -> Just s)   = s
{-
                    ^^^^^^^^^^^^^^^^^^^^
                             |
                             +--<< これは [(a,b)] の引数なので、lookup k [(a,b)] のように解釈され
                                   その結果、Just s であれば s が戻る
-}
lookupDefault _ d _ = d

-- lookupDefault 100 "*no exist*" [(100, "val is 100")]
-- lookupDefault 200 "*no exist*" [(100, "val is 100")]


-- pattern guard

lookupDefault' :: Eq a => a -> b -> [(a,b)] -> b

lookupDefault' k _ xs
    | (Just s) <- lookup k xs = s

lookupDefault' _ d _ = d

