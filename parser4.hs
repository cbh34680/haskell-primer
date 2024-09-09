import Control.Exception
import Control.Monad.State
import Control.Monad
import Data.Char
import Data.Bool


{-
parseTest f cs =
    case (evalStateT f cs) of
        Right _ -> print cs
        Left err -> print ("error: " ++ err ++ ": " ++ cs)
-}
parseTest (StateT f) cs =
    case (f cs) of
        Right dbg -> putStrLn (cs ++ " {DBG: " ++ show dbg ++ "}")
        Left err -> putStrLn ("error: " ++ err ++ ": " ++ cs)


type EitherS = Either String

anyChar :: StateT String EitherS Char
anyChar = StateT $ f
    where
        f [] = Left "empty"
        f (c:cs) = Right (c, cs)


satisfy :: (Char -> Bool) -> StateT String EitherS Char
satisfy f = anyChar >>= \a -> guardT (f a) >> return a

satisfy' f = do
    a <- anyChar
    guardT (f a)
    --lift $ if f a then pure () else Left "guard"
    return a


guardT :: Bool -> StateT String EitherS ()
guardT b = lift $ f b
    where
        f True = pure ()
        f False = Left "guard"
    

orT :: StateT String EitherS Char -> StateT String EitherS Char -> StateT String EitherS Char
a `orT` b = StateT $ \s ->
    let
        ea = runStateT a s      -- ea : EitterS a
        eb = runStateT b s      -- eb : EitherS a
    in
        f ea eb
    where
        f (Left msga) (Left msgb) = Left (msga ++ "/" ++ msgb)
        f (Left _) b' = b'
        f a' _ = a'


digit = satisfy isDigit `orT` lift (Left "[!digit]")
--digit = satisfy isDigit


test1 = do
    a <- anyChar
    b <- anyChar

    return [a,b]


main = do
    parseTest anyChar "abcde"
    parseTest anyChar ""
    parseTest test1 "a"
    parseTest test1 "abcde"
    parseTest (satisfy (== 'A')) "abcde"
    parseTest (satisfy (== 'A')) ""
    parseTest digit "a"
    parseTest digit "1"


-- EOF
