import Control.Applicative ((<$>), (<*>))
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
    

-- orT :: StateT String EitherS Char -> StateT String EitherS Char -> StateT String EitherS Char
orT :: StateT String EitherS a -> StateT String EitherS a -> StateT String EitherS a
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

alpha = satisfy isAlpha `orT` lift (Left "[!alpha]")
letter = satisfy isLetter `orT` lift (Left "[!letter]")


many' :: StateT String EitherS Char -> StateT String EitherS String
many' f = StateT $ \s -> return $ span g s
    where
        g c = either (const False) (const True) (runStateT f [c])


many :: StateT String EitherS Char -> StateT String EitherS String
many p = ((:) <$> p <*> many p) `orT` return []



test1 = do
    a <- anyChar
    b <- anyChar

    return [a,b]


test7 = many letter
test8 = many (letter `orT` digit)


main = do
    parseTest (digit `orT` alpha)  "a"
    parseTest (digit `orT` alpha)  "1"
    parseTest (digit `orT` alpha)  "!"
    parseTest test7 "abc123"
    parseTest test7 "123abc"
    parseTest test8 "abc123"
    parseTest test8 "123abc"


-- EOF
