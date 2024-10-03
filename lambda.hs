-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}

import Control.Applicative
import Control.Monad
--import Control.DeepSeq
import Debug.Trace

--import Control.Monad.Trans.Maybe
--import Control.Monad.Except
--import Control.Monad.Reader
--import Control.Monad.Trans.Class (lift)

import Data.Foldable (traverse_)
import Data.Char (isAsciiLower, isSpace)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List ((\\), group, sort, intersperse)
import Data.List.Extra (notNull)

import qualified Data.Map as Map
import qualified Text.Parsec as P


data Term =
    Label String |
    Bound Char |
    Var String |
    Function {bound'::Char, body'::Term} |
    Apply {func'::Term, arg'::Term} deriving (Eq, Show)

data Stmt = Define (String, Term) | Expr Term deriving (Eq, Show)

type Defines = Map.Map String Term

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


commentLine = do
    P.string "--"
    --many (P.noneOf "\n")
    many (P.noneOf "\n")
    return Nothing


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

    --print $ Map.fromList defs
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

    let db = Map.fromList defs

    -- print expr
    -- print db
    let extExprs = map (\(Expr x) -> extract 0 db x) exprs
    traverse_ (putStrLn . show) extExprs

    putStrLn ""
    mapM_ (putStrLn . showLambda) extExprs
    putStrLn ""
    mapM_ (putStrLn . showHaskell) extExprs
    putStrLn ""

    putStrLn "---------- complete ----------"
    putStrLn ""


-- #--------------------------------------------------------------------------
-- #
-- #        e x t r a c t
-- #
-- #--------------------------------------------------------------------------

extract :: Int -> Defines -> Term -> Term

extract lv db (Var key) = do
    --let !_ = trace (mkdbg lv "Var" [ "<" ,key ]) 1

    let term = case Map.lookup key db of
                    Just x  -> extract (lv + 1) db x
                    Nothing -> Label key

    --let !_ = trace (mkdbg lv "Var" [ "<" ,showType term ++ ":" ++ showLambda term ]) 1

    term


extract lv db (Function c term) = do
    --let !_ = trace (mkdbg lv "Fun" [ "<" ,[c] ,showType term ++ ":" ++ showLambda term ]) 1

    let db' = Map.insert [c] (Bound c) db
    let term' = extract (lv + 1) db' term

    --let !_ = trace (mkdbg lv "Fun" [ "<" ,[c] ,showType term' ++ ":" ++ showLambda term' ]) 1

    Function c term'


extract lv db (Apply lt rt) = do
    --let !_ = trace (mkdbg lv "App" [ "<" ,showType lt ++ ":" ++ showLambda lt ,showType rt ++ ":" ++ showLambda rt ]) 1

    let lt' = extract (lv + 1) db lt
    let rt' = extract (lv + 1) db rt

    --let !_ = trace (mkdbg lv "App" [ ">" ,showType lt'  ++ ":" ++ showLambda lt' ,showType rt' ++ ":" ++ showLambda rt' ]) 1

    Apply lt' rt'


extract lv db org@(Bound c) = do
    --let !_ = trace (mkdbg lv "Bou" [ ">" ,[c] ]) 1

    org


extract lv db org@(Label key) = do
    --let !_ = trace (mkdbg lv "Lab" [ ">" ,key ]) 1

    let !_ = error "Invalid Term Type !!"

    org


--mkdbg lv t xs = (indent lv) ++ (mconcat . (intersperse "|") $ ["|", t] ++ xs ++ ["|"])

mkdbg lv t = (++) (indent lv) . mconcat . (intersperse "|") . (++) ["|", t] . flip (++) ["|"]

indent :: Int -> String
indent n = replicate (n * 2) ' '


main = do
    {-
    print $ Function "f" (Function "g" (Function "x" (Apply (Var "f") (Apply (Var "g") (Var "x")))))
    print $ Function "f" (Function "g" (Function "x" (Apply (Apply (Var "f") (Var "g")) (Var "x"))))
    -}
    --P.parse parser "(src)" input

    input <- readFile "a.lmd"

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
