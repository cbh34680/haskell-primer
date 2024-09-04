{-# LANGUAGE LambdaCase #-}

import Data.Char
import Control.Monad.State


getch :: (Char -> Bool) -> StateT String Maybe Char
--getch :: (a -> Bool) -> StateT [a] Maybe a
getch f = StateT $ \case
        [] -> Nothing
        (c:cs) -> if f c then Just (c, cs) else Nothing


test :: String -> Maybe (String, String)
test s0 = runStateT steps s0
    where
        steps = do
            ch1 <- getch isUpper
            ch2 <- getch isLower
            ch3 <- getch isDigit

            return [ch1, ch2, ch3]

main = do
    print $ test "Aa0"  -- OK
    print $ test "abc"  -- エラー


