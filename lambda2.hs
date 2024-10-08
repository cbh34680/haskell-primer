-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

-- :set -DNOT_DIVE

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Debug.Trace
import Text.Show.Pretty (ppShow)

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
    Var String |
    Lambda {mBbound::String, mBbody::Term} |
    Apply {mFunc::Term, mAarg::Term} deriving (Eq, Show, Generic)

instance NFData Term

data Stmt = Define (String, Term) | Expr Term deriving (Eq, Show)

type Indent = Int

showLambdaKey = takeWhile (/= '#')

showLambda (Lambda key term) = mconcat ["(λ", showLambdaKey key, ".", showLambda term, ")"]
showLambda (Apply lt rt) = mconcat ["(", showLambda lt, " ", showLambda rt, ")"]
showLambda (Var key) = showLambdaKey key

showType (Var _) = "V"
showType (Lambda _ _) = "L"
showType (Apply _ _) = "A"

showHaskell (Lambda key term) = mconcat ["(\\", key, " -> ", showHaskell term, ")"]
showHaskell (Apply lt rt) = mconcat ["(", showHaskell lt, " ", showHaskell rt, ")"]
showHaskell (Var key) = key


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


parseTerm = parseNested <|> parseLambda <|> parseVar


parseNested = literal '(' *> parseApply <* literal ')'

--parseLambda = Lambda <$> (literal '\\' *> parseLetter <* literal '.') <*> parseApply
{-
parseLambda = do
    literal '\\'
    cs <- parseLetter
    literal '.'

    Lambda [cs] <$> parseApply
-}
parseLambda = do
    --literal '\\'
    P.oneOf "\\λ"
    skipSpaces

    cs <- filter (not . isSpace) <$> parseArgs
    literal '.'
    apply <- parseApply

    let (s:ss) = map (\x -> [x]) $ reverse cs

    --return $ foldl (\acc c -> Lambda c acc) (Lambda x apply) xs
    return $ foldl (flip Lambda) (Lambda s apply) ss


parseVar = Var <$> parseIdent


executeStmts stmts = do
    let isExpr :: Stmt -> Bool
        isExpr (Expr _) = True
        isExpr _ = False

    let exprs' = filter isExpr stmts
    let defs = map (\(Define x) -> x) $ stmts \\ exprs'

    let dups = map (!! 0) . filter ((> 1) . length) . group . sort $ map fst defs
    when (notNull dups) (error (mconcat ["duplicate terms (", show dups, ")"]))
    --guard (notnull dups)

    let exprs = map (\(Expr x) -> x) $ exprs'

    putStrLn "---------- haskell ----------"
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> show . snd)) defs
    putStrLn ""
    traverse_ print exprs
    putStrLn ""
    putStrLn "---------- define ----------"
    traverse_ (putStrLn . ((\k v -> k ++ " = " ++ v) <$> fst <*> showLambda . snd)) defs
    putStrLn ""
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> showHaskell . snd)) defs
    putStrLn ""
    putStrLn "---------- expr ----------"
    pPrint $ exprs
    putStrLn ""
    traverse_ (putStrLn . showLambda) exprs
    putStrLn ""
    traverse_ (putStrLn . showHaskell) exprs
    putStrLn ""

    -- alpha
    let (aDefs, lastId) = runState (mapAlphaDefines defs) 0
    let (aExprs, _) = runState (mapAlphaExprs exprs) lastId

    putStrLn "---------- alpha define ----------"
    traverse_ (putStrLn . ((\k v -> k ++ " = " ++ v) <$> fst <*> showLambda . snd)) aDefs
    putStrLn ""
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> showHaskell . snd)) aDefs
    putStrLn ""
    putStrLn "---------- alpha expr ----------"
    pPrint $ aExprs
    putStrLn ""
    traverse_ (putStrLn . showLambda) aExprs
    putStrLn ""
    traverse_ (putStrLn . showHaskell) aExprs
    putStrLn ""

    putStrLn ""
    putStrLn "---------- complete ----------"
    putStrLn ""



mapAlphaDefines :: [(String, Term)] -> State Int [(String, Term)]

mapAlphaDefines [] = return []

mapAlphaDefines ((name, expr):defs) =
    (:) <$> ((name, ) <$> alpha [] expr) <*> mapAlphaDefines defs


mapAlphaExprs :: [Term] -> State Int [Term]

mapAlphaExprs [] = return []

mapAlphaExprs (expr:defs) = (:) <$> alpha [] expr <*> mapAlphaExprs defs



pPrint = putStrLn . ppShow

-- #--------------------------------------------------------------------------
-- #
-- #
-- #
-- #--------------------------------------------------------------------------

genId :: State Int Int
genId = modify (+1) >> get


alpha :: [(String, String)] -> Term -> State Int Term

alpha db (Apply lt rt) = do
    lt' <- alpha db lt
    rt' <- alpha db rt

    return $ Apply lt' rt'


alpha db (Lambda key term) = do
    newId <- genId

    let key' = key ++ ('#' : (show newId))
    let db' = (key, key'):db

    term' <- alpha db' term

    return $ Lambda key' term'
    

alpha db org@(Var key) = do
    case lookup key db of
        Just key' -> return $ Var key'
        Nothing   -> return org

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
    input <- readFile "example.lmd"

    case P.parse parser "(src)" input of
        Right stmts -> executeStmts stmts
        Left err -> putStrLn "[parse error]" >> print err


    putStrLn "done."


-- EOF
