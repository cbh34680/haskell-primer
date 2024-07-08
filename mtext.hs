import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Maybe as M


trimLeft::String -> String
trimLeft = dropWhile C.isSpace

trimRight::String -> String
trimRight = foldr f []
    where
        f x [] | C.isSpace x = []
        f x ys = x : ys

trim::String -> String
trim = trimLeft . trimRight

substring::Int -> Int -> String -> String
substring s l = take l . drop s

slice::Int -> Int -> [a] -> [a]
slice s e = take (e - s) . drop s

isSubject::String -> Bool
isSubject cs
    | L.isPrefixOf "*** " cs = L.isSuffixOf " ***" cs
    | otherwise = False

isBody::String -> Bool
isBody = not . isSubject

subject::String -> String
subject cs = trim $ slice 4 (length cs - 4) cs

data Article = Article { getBody::[String], getSubject::String }

instance Show Article where
    show a = "[" ++ getSubject a ++ "]\n" ++ ( unlines $ map ('\t':) $ getBody a )

articles::[String] -> [Article]
articles css = f css Nothing
    where
        f [] _ = []

        f (line:rest) Nothing  | isBody line = f rest Nothing
        f (line:rest) (Just a) | isBody line = f rest (Just $ a { getBody=(line : (getBody a)) })

        f (line:rest) m
            | M.isNothing m = g
            | otherwise     = let Just a = m in a { getBody=(reverse $ getBody a) } : g
            where
                g = f rest (newArticle line)

        newArticle cs
            | null subj = Nothing
            | otherwise = Just $ Article [] subj
            where
                subj = subject cs

main = do
    css <- getContents >>= return . lines
    mapM_ print $ articles css

    print "done."

-- EOF
