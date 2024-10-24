
import Control.Monad.State
import Control.Monad
import Control.Applicative


getch :: StateT String Maybe Char
getch = StateT $ \s -> case s of
                            []     -> Nothing
                            (c:cs) -> Just (c, cs)
    


get3 :: String -> Maybe (String, String)
get3 cs = runStateT step cs
    where
        step = do
            a <- getch
            b <- getch
            c <- getch
            return [a, b, c]
    



main = do
    print $ get3 "abcd"  -- OK
    print $ get3 "1234"  -- OK
    print $ get3 "a"     -- NG

