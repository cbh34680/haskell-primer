-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

-- :set -DNOT_DIVE

import Control.Applicative
import Control.Monad
import Debug.Trace

import GHC.Generics (Generic)
import Control.DeepSeq

--import Control.Monad.Trans.Maybe
--import Control.Monad.Except
--import Control.Monad.Reader
--import Control.Monad.Trans.Class (lift)

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Char (isAsciiLower, isSpace)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List ((\\), group, sort, intersperse)
import Data.List.Extra (notNull)

import qualified Text.Parsec as P


data Term =
    Label String |
    Bound Char |
    Var String |
    Function {bound'::Char, body'::Term} |
    Apply {func'::Term, arg'::Term} deriving (Eq, Show, Generic)

instance NFData Term

data Stmt = Define (String, Term) | Expr Term deriving (Eq, Show)

type TermStack = [(String, Term)]

type Indent = Int

showLambda (Function c term) = mconcat ["(\\", [c], ".", showLambda term, ")"]
showLambda (Apply t1 t2) = mconcat ["(", showLambda t1, " ", showLambda t2, ")"]
showLambda (Var cs) = cs
showLambda (Label cs) = cs
showLambda (Bound c) = [c]

showType (Var _) = "V"
showType (Label _) = "L"
showType (Bound _) = "B"
showType (Function _ _) = "F"
showType (Apply _ _) = "A"

{-
showHaskell (Label cs) = cs
showHaskell (Bound c) = [c]
showHaskell (Var cs) = cs
-}
showHaskell (Function c term) = mconcat ["(\\", [c], " -> ", showHaskell term, ")"]
showHaskell (Apply t1 t2) = mconcat ["(", showHaskell t1, " ", showHaskell t2, ")"]
showHaskell x = showLambda x


separator = P.oneOf " \t"
skipSpaces = void (P.skipMany separator)


--parseIdent = P.many1 P.letter <* skipSpaces
{-
parseIdent = do
    x <- P.letter
    xs <- P.many (P.letter <|> P.digit)

    skipSpaces

    return (x:xs)
-}
parseIdent = ((:) <$> P.letter <*> P.many (P.letter <|> P.digit)) <* skipSpaces


parseArgs = P.many1 (P.satisfy isAsciiLower <|> P.char ' ') <* skipSpaces
-- parseLetter = P.letter <* skipSpaces

eol = P.char '\n'

literal c = P.char c <* skipSpaces


--parser :: P.Parsec String () Term
--parser = skipSpaces *> (catMaybes <$> parseStmt `P.sepBy` (P.many separator)) <* P.eof
parser = (catMaybes <$> P.many parseStmt) <* P.eof


--parseTerm :: P.Parsec String () Term
parseStmt = skipSpaces *>
    (P.try parseDefine <|> commentLine <|> parseExpr <|> emptyLine) <* eol

--parseStmt = (P.try parseDefine <|> parseExpr <|> emptyLine) <* eol

{-
parseDefine = do
    ident <- parseIdent
    literal '='
    apply <- parseApply

    return $ Just $ Define (ident, apply)
-}
--parseDefine = Just . Define <$> ((,) <$> (parseIdent <* literal '=') <*> parseApply)
parseDefine = ((Just . Define) .) . (,) <$> (parseIdent <* literal '=') <*> parseApply


parseExpr = Just . Expr <$> parseApply


{-
commentLine = do
    P.string "--"
    --many (P.noneOf "\n")
    many (P.noneOf "\n")
    return Nothing
-}
--commentLine = P.string "--" *> many (P.noneOf "\n") *> return Nothing
--commentLine = (P.string "--" *> many (P.noneOf "\n")) $> Nothing
commentLine = Nothing <$ (P.string "--" *> many (P.noneOf "\n"))


-- emptyLine = skipSpaces *> return Nothing
emptyLine = Nothing <$ skipSpaces


parseApply = do
    term <- parseTerm

    do
        terms <- P.many1 parseTerm
        --return $ foldl (\ls x -> Apply ls x) term terms
        return $ foldl Apply term terms

        <|> do
            return term


parseTerm = parseNested <|> parseFunction <|> parseVar


parseNested = literal '(' *> parseApply <* literal ')'

--parseFunction = Function <$> (literal '\\' *> parseLetter <* literal '.') <*> parseApply
{-
parseFunction = do
    literal '\\'
    cs <- parseLetter
    literal '.'

    Function [cs] <$> parseApply
-}
parseFunction = do
    literal '\\'
    cs <- filter (not . isSpace) <$> parseArgs
    literal '.'
    apply <- parseApply

    let (x:xs) = reverse cs

    --return $ foldl (\acc c -> Function c acc) (Function x apply) xs
    return $ foldl (flip Function) (Function x apply) xs


parseVar = Var <$> parseIdent

isExpr :: Stmt -> Bool
isExpr (Expr _) = True
isExpr _ = False

executeStmts stmts = do
    let exprs = filter isExpr stmts
    let defs = map (\(Define t) -> t) $ stmts \\ exprs

    let dups = map (!! 0) . filter ((> 1) . length) . group . sort $ map fst defs
    when (notNull dups) (error (mconcat ["duplicate terms (", show dups, ")"]))
    --guard (notnull dups)

    --print exprs

    putStrLn "---------- haskell ----------"
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> show . snd)) defs
    putStrLn ""
    traverse_ print exprs
    putStrLn ""
    putStrLn "---------- define ----------"
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> showLambda . snd)) defs
    putStrLn ""
    putStrLn "---------- expr ----------"
    print exprs
    putStrLn ""
    putStrLn "---------- extract ----------"

    {-
        Var により参照されている Term に置き換える

        例)
            zero = \f. \x. x
            succ = \n. \f. \x. f (n f x)
            succ zero

        の定義が

            (\n. \f. \x. f (n f x)) (\f. \x. x)

        に書き換えられ、参照先のないものは Label になる

        変換前の Term の種類) Function, Apply, Var
        変換後の Term の種類) Function, Apply, Label, Bound
    -}

    -- print expr
    -- print db
    let extExprs = map (\(Expr x) -> extract 0 defs x) exprs
    let !_ = extExprs `deepseq` ()

    --traverse_ (putStrLn . show) extExprs
    traverse_ print extExprs

    putStrLn ""
    traverse_ (putStrLn . showLambda) extExprs
    putStrLn ""
    traverse_ (putStrLn . showHaskell) extExprs
    putStrLn ""

    {-
    putStrLn "---------- reduction ----------"

    let redExprs = map (reduction 0 []) extExprs
    let !_ = redExprs `deepseq` ()

    putStrLn ""

    traverse_ print redExprs
    putStrLn ""
    traverse_ (putStrLn . showLambda) redExprs
    putStrLn ""
    traverse_ (putStrLn . showHaskell) redExprs
    putStrLn ""

    putStrLn "---------- check ----------"
    --print $ map diveCalc extExprs
    --print $ map diveCalc redExprs

    putStrLn "!!! aa !!!"
    let aa = map (reduction 0 []) redExprs
    let !_ = aa `deepseq` ()
    putStrLn ""
    traverse_ print aa
    putStrLn ""
    traverse_ (putStrLn . showLambda) aa
    putStrLn ""
    traverse_ (putStrLn . showHaskell) aa
    putStrLn ""

    putStrLn "!!! bb !!!"
    let bb = map (reduction 0 []) aa
    let !_ = bb `deepseq` ()
    putStrLn ""
    traverse_ print bb
    putStrLn ""
    traverse_ (putStrLn . showLambda) bb
    putStrLn ""
    traverse_ (putStrLn . showHaskell) bb
    putStrLn ""

    putStrLn "!!! cc !!!"
    let cc = map (reduction 0 []) bb
    let !_ = cc `deepseq` ()
    putStrLn ""
    traverse_ print cc
    putStrLn ""
    traverse_ (putStrLn . showLambda) cc
    putStrLn ""
    traverse_ (putStrLn . showHaskell) cc
    putStrLn ""

    putStrLn "!!! dd !!!"
    let dd = map (reduction 0 []) cc
    let !_ = dd `deepseq` ()
    putStrLn ""
    traverse_ print dd
    putStrLn ""
    traverse_ (putStrLn . showLambda) dd
    putStrLn ""
    traverse_ (putStrLn . showHaskell) dd
    putStrLn ""

    -}

    putStrLn "---------- complete ----------"
    putStrLn ""



-- #--------------------------------------------------------------------------
-- #
-- #        r e d u c t i o n
-- #
-- #--------------------------------------------------------------------------

hasChild :: Term -> Bool
hasChild (Function _ _) = True
hasChild (Apply _ _) = True
hasChild _ = False


deleteFirst :: Eq a => a -> [(a,b)] -> [(a,b)]

--deleteFirst _ = id

deleteFirst x [] = []
deleteFirst x (y@(k,_):ys)
    | x == k = ys
    | otherwise = y: deleteFirst x ys


reduction :: Indent -> TermStack -> Term -> Term

reduction lv db (Apply lt@(Function c term) rt) = do
    --let !_ = trace (mkdbg lv "ApF" [ "<" ,[c],showType lt ++ ":" ++ showLambda lt ,showType rt ++ ":" ++ showLambda rt ]) 1

    let db' = (([c], rt):db)

    reduction (lv + 1) db' term


reduction lv db org@(Bound c) = do
    --let !_ = trace (mkdbg lv "Bou" [ "<" ,[c] ]) 1

    case lookup [c] db of
        Nothing -> org

        {-
            "iszero c0" のときに無限ループする
        -}
        --Just x -> reduction (lv + 1) db x

        {-
            引数リストから削除 ... 影響は ?
        Just x -> do
                let db' = deleteFirst [c] db
                reduction (lv + 1) db' x
        -}

        {-
            "(\f.\x. f x) (\y.y) z" のときに (Label "z') ではなく (Bound 'x') となってしまう
        -}
        Just x -> x

        {-
            折衷案
        Just x -> case hasChild x of
                    True -> x
                    _    -> reduction (lv + 1) db x
        -}


#if defined(NOT_DIVE)

reduction lv db (Apply lt rt) = do
    let !_ = trace (mkdbg lv "App" [ "<" ,showType lt, ":", showLambda lt ,showType rt, ":", showLambda rt ]) 1

    let lt' = reduction (lv + 1) db lt
    let rt' = reduction (lv + 1) db rt

    Apply lt' rt'


reduction lv db (Function c term) = do
    let !_ = trace (mkdbg lv "Fun" [ "<" ,[c] ,showType term, ":", showLambda term ]) 1

    let term' = reduction (lv + 1) db term

    Function c term'


reduction lv db org = do
    let !_ = trace (mkdbg lv "Non" [ "<" ,showType org, ":", showLambda org ]) 1

    org

#else

reduction lv db term = do
    --let !_ = trace (mkdbg lv "Via" [ "<" ,showType term, ":", showLambda term ]) 1

    dive reduction lv db term

#endif


-- #--------------------------------------------------------------------------
-- #
-- #        e x t r a c t
-- #
-- #--------------------------------------------------------------------------

extract :: Indent -> TermStack -> Term -> Term

extract lv db (Var key) = do
    --let !_ = trace (mkdbg lv "Var" [ "<" ,key ]) 1

    let term = case lookup key db of
                    Just x  -> extract (lv + 1) db x

                    {-
                        引数リストから削除 ... 影響は ?
                    Just x  -> do
                                let db' = deleteFirst key db
                                extract (lv + 1) db' x
                    -}

                    Nothing -> Label key

    --let !_ = trace (mkdbg lv "Var" [ "<" ,showType term ++ ":" ++ showLambda term ]) 1

    term


extract lv db (Function c term) = do
    --let !_ = trace (mkdbg lv "Fun" [ "<" ,[c] ,showType term ++ ":" ++ showLambda term ]) 1

    let db' = (([c], (Bound c)):db)
    let term' = extract (lv + 1) db' term

    --let !_ = trace (mkdbg lv "Fun" [ "<" ,[c] ,showType term' ++ ":" ++ showLambda term' ]) 1

    Function c term'


extract lv db org@(Label key) = do
    --let !_ = trace (mkdbg lv "Lab" [ ">" ,key ]) 1

    let !_ = error "Invalid Term Type !!"

    org


#if defined(NOT_DIVE)

extract lv db (Apply lt rt) = do
    --let !_ = trace (mkdbg lv "App" [ "<" ,showType lt ++ ":" ++ showLambda lt ,showType rt ++ ":" ++ showLambda rt ]) 1

    let lt' = extract (lv + 1) db lt
    let rt' = extract (lv + 1) db rt

    --let !_ = trace (mkdbg lv "App" [ ">" ,showType lt'  ++ ":" ++ showLambda lt' ,showType rt' ++ ":" ++ showLambda rt' ]) 1

    Apply lt' rt'


extract lv db org@(Bound c) = do
    --let !_ = trace (mkdbg lv "Bou" [ ">" ,[c] ]) 1

    org

# else

extract lv db term = do
    --let !_ = trace (mkdbg lv "Via" [ "<" ,showType term, ":", showLambda term ]) 1

    dive extract lv db term

#endif


-- #--------------------------------------------------------------------------
-- #
-- #        d i v e   c a l c
-- #
-- #--------------------------------------------------------------------------

diveCalc :: Term -> Int

diveCalc (Apply lt@(Function c term) rt) = (diveCalc lt) + (diveCalc rt) + 1

diveCalc (Apply lt rt) = (diveCalc lt) + (diveCalc rt)

diveCalc (Function c term) = diveCalc term

diveCalc term = 0


-- #--------------------------------------------------------------------------
-- #
-- #        d i v e
-- #
-- #--------------------------------------------------------------------------

#if !defined(NOT_DIVE)

dive :: (Indent -> TermStack -> Term -> Term) -> Indent -> TermStack -> Term -> Term

dive next lv db (Apply lt rt) = do
    --let !_ = trace (mkdbg lv "App" [ "<" ,showType lt, ":", showLambda lt ,showType rt, ":", showLambda rt ]) 1

    let lt' = next (lv + 1) db lt
    let rt' = next (lv + 1) db rt

    Apply lt' rt'


dive next lv db (Function c term) = do
    --let !_ = trace (mkdbg lv "Fun" [ "<" ,[c] ,showType term, ":", showLambda term ]) 1

    let term' = next (lv + 1) db term

    Function c term'


dive next lv db term = do
    --let !_ = trace (mkdbg lv "Non" [ "<" ,showType org, ":", showLambda org ]) 1

    term

#endif


-- #--------------------------------------------------------------------------
-- #
-- #
-- #
-- #--------------------------------------------------------------------------

--mkdbg lv t xs = (indentStr lv) ++ (mconcat . (intersperse "|") $ ["|", t] ++ xs ++ ["|"])

mkdbg lv t = (++) (indentStr lv) . mconcat . intersperse "|" . (++) ["|", t] . flip (++) ["|"]

indentStr :: Indent -> String
indentStr n = replicate (n * 2) ' '


main = do
    {-
    print $ Function "f" (Function "g" (Function "x" (Apply (Var "f") (Apply (Var "g") (Var "x")))))
    print $ Function "f" (Function "g" (Function "x" (Apply (Apply (Var "f") (Var "g")) (Var "x"))))
    -}
    --P.parse parser "(src)" input

#if defined(NOT_DIVE)
    putStrLn "no dive"

#else
    putStrLn "use dive"

#endif

    input <- readFile "example.lmd"

    case P.parse parser "(src)" input of
        Right stmts -> executeStmts stmts
        Left err -> putStrLn "[parse error]" >> print err


    putStrLn "done."


{-
input = "\
\  (\\f. f) (\\x. x)  \n\
\"

input' = "\
\  \n\
\  succ  = \\n. \\f. \\x. f (n f x)   \n\
\  c0    = \\f. \\x. x                \n\
\  c1    = succ c0                    \n\
\  c2    = succ (succ c0)             \n\
\  \n\
\  c1    \n\
\"
-}


-- EOF
