import qualified Data.Char as C
import Control.Applicative
import Control.Monad

-- ReadS ... String -> [(a, String)]

newtype Parser a = Parser { parse :: ReadS a }


-- fmap toUpper item :: Parser a

instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> [(f x, s') | (x, s') <- p s]


item = Parser $ \(c:cs) -> [(c, cs)]
failure = Parser $ \_ -> []


f = parse (item) "abcde"
f' = parse (failure) "abcde"


instance Applicative Parser where
    pure x = Parser $ \s -> [(x, s)]
    (Parser p) <*> (Parser q) = Parser $ \s -> [(f x, s'') | (f, s') <- p s, (x, s'') <- q s']


g = parse (pure (,) <*> item <*> item) "abcde"



instance Monad Parser where
    Parser m >>= f = Parser $ \s -> [(y, s'') | (x, s') <- m s, (y, s'') <- parse (f x) s']


toUpperM c = Parser $ \s -> [(C.toUpper c, s)]
toStringM c = Parser $ \s -> [([c], s)]


h = parse (item >>= toUpperM >>= toStringM) "abcde"



{-
instance Semigroup a => Semigroup (Parser a) where
    (Parser p) <> (Parser q) = Parser $ \s -> [(x <> y, s'') | (x, s') <- p s, (y, s'') <- q s']
-}
instance Semigroup (Parser a) where
    (Parser p) <> (Parser q) = Parser $ \s -> case p s of
                                                [] -> q s
                                                x -> x


i = parse ((item >>= toStringM) <> (item >>= toStringM)) "abcde"

i' = parse ((item >>= toStringM) <> failure) "abcde"
i'' = parse (failure <> (item >>= toStringM)) "abcde"

j = parse (pure 'A' <> item) "abcde"
j' = parse (item <> pure 'A') "abcde"

j'' = parse (failure <> item) "abcde"
j''' = parse (item <> failure) "abcde"




instance Monoid (Parser a) where
    --mempty = Parser $ \s -> []
    mempty = Parser $ const []


k = parse (mempty <> item <> item) "abcde"
k' = parse (mempty <> mempty <> item) "abcde"



instance Alternative Parser where
    empty = mempty
    (<|>) = (<>)


l = parse (empty <|> item) "abcde"
l' = parse (item <|> empty) "abcde"

l'' = parse (item >>= \x -> guard (x `elem` "ab") >> return x) "abcde"
l''' = parse (item >>= \x -> guard (x `elem` "AB") >> return x) "abcde"













-- EOF
