import Data.Bool
import Control.Monad.Trans.State
import Control.Monad.Trans.Class

checkParent :: Int -> String -> Bool

checkParent s [] = s == 0

checkParent s (c:cs)
    | s >= 0 =
        case c of
            '(' -> checkParent (s + 1) cs
            ')' -> checkParent (s - 1) cs
            _   -> checkParent s cs
    | otherwise = False


f = checkParent 0 "(aaa (bbb ccc))"

----------------------------------

type StateData = (String, Int)

checkParentS :: State StateData Bool

checkParentS = do
    (cs, s) <- get

    if s >= 0 then
        case cs of
            [] -> return $ s == 0
            (c:cs') -> do
                case c of
                    '(' -> put (cs', s + 1)
                    ')' -> put (cs', s - 1)
                    _   -> put (cs', s)
                checkParentS
    else
        return False


f' = runState checkParentS  ("(aaa (bbb ccc))", 0)


----------------------------------

type Count = Int
type StateData2 = (String, Int, Count)


checkParentS2 :: State StateData2 Bool

checkParentS2 = do
    (cs, s, n) <- get

    if s >= 0 then
        case cs of
            [] -> return $ s == 0
            (c:cs') -> do
                case c of
                    '(' -> put (cs', s + 1, n + 1)
                    ')' -> put (cs', s - 1, n + 1)
                    _   -> put (cs', s,     n + 1)
                checkParentS2
    else
        return False


f'' = runState checkParentS2 ("(aaa (bbb ccc))", 0, 0)


----------------------------------

checkParentST :: StateT StateData2 IO Bool
checkParentST = do
    (cs, s, n) <- get

    lift $ putStrLn cs

    if s >= 0 then
        case cs of
            [] -> return $ s == 0
            (c:cs') -> do
                case c of
                    '(' -> put (cs', s + 1, n + 1)
                    ')' -> put (cs', s - 1, n + 1)
                    _   -> put (cs', s,     n + 1)
                checkParentST
    else
        return False


g = runStateT checkParentST ("(aaa (bbb ccc))", 0, 0)




-- EOF
