import qualified Data.Char as C

-- ReadS ... String -> [(a, String)]

newtype Parser a = Parser { parse :: ReadS a }


-- fmap toUpper item :: Parser a

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> [(f x, s') | (x, s') <- p s]


item = Parser $ \(c:cs) -> [(c, cs)]
failure = Parser $ \_ -> []


f = parse (item) "abcde"
g = parse (failure) "abcde"


instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    (Parser p) <*> (Parser q) = Parser $ \s -> [(f x, s'') | (f, s') <- p s, (x, s'') <- q s']


f' = parse (pure (,) <*> item <*> item) "abcde"



instance Monad Parser where
    Parser m >>= f = Parser $ \s -> [(y, s'') | (x, s') <- m s, (y, s'') <- parse (f x) s']


toUpperM c = Parser $ \s -> [(C.toUpper c, s)]
stringsM c = Parser $ \s -> [([c], s)]


f'' = parse (item >>= toUpperM >>= stringsM) "abcde"






-- EOF
