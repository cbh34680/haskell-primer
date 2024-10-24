import qualified Data.Char as C
import qualified Data.List as L
import qualified Data.Maybe as MB
import System.IO

main = do
    cs <- readFile "./config.txt"
    
    let ckvs = categorisedKeyValues . filter notComment . lines $ cs
    let enc = lookup "database" ckvs >>= lookup "encoding"

    print enc

    print "done."


categorisedKeyValues = map h . L.groupBy g . MB.catMaybes . map (f . break (== '='))
    where
        f :: (String, String) -> Maybe (String, (String, String))
        f (ckey, ('=':rest)) = f' (break (== '.') $ trim ckey) (trim rest)
        f _ = Nothing

        f' :: (String, String) -> String -> Maybe (String, (String, String))
        f' (cat, ('.':key)) val = Just (cat, (key, val))
        f' _ _ = Nothing

        g = (\(l,_) (r,_) -> l == r)

        h (x:xs) = (fst x, snd x: h' xs)

        h' (x:xs) = snd x : h' xs
        h' [] = []



notComment :: String -> Bool
notComment [] = False
notComment (x:_)
    | x /= '#' = True
    | otherwise = False

--

trimLeft::String -> String
trimLeft = dropWhile C.isSpace

trimRight::String -> String
trimRight = foldr f []
    where
        f x [] | C.isSpace x = []
        f x ys = x : ys

trim = trimLeft . trimRight


--
