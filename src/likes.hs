import Data.Bool

type Results a = [(Bool, (a, a))]

results :: Results String
results = [
    (True, ("a", "b")),
    (False, ("c", "d")),
    (False, ("e", "f")) ]


likes :: Results a -> [a]
likes = map . uncurry $ bool snd fst

likes' :: Results a -> [a]
likes' = map $ \(b, t) -> bool snd fst b t

f = likes results
f' = likes' results
