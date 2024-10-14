-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

-- :set -DNOT_DIVE

--import Test.QuickCheck

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Control.Arrow (first, second)
import Control.Monad.State
import Control.Monad.Except

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData, deepseq)

--import Control.Monad.Trans.Maybe
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
    Bnd String |
    Fun {mKey::String, mBbody::Term} |
    App {mLT::Term, mRT::Term}
    deriving (Eq, Show, Generic)

instance NFData Term

data Stmt = Define (String, Term) | Expr Term deriving (Eq, Show)

type Indent = Int

showLambdaKey = takeWhile (/= '{')
--showLambdaKey = id

showLambda (Fun bnd body) = mconcat ["(λ", showLambdaKey bnd, ".", showLambda body, ")"]
showLambda (App lt rt) = mconcat ["(", showLambda lt, " ", showLambda rt, ")"]
showLambda (Var key) = showLambdaKey key
showLambda (Bnd key) = showLambdaKey key

showType (Var _) = "V"
showType (Bnd _) = "B"
showType (Fun _ _) = "F"
showType (App _ _) = "A"

showHaskell (Fun bnd body) = mconcat ["(\\", showLambdaKey bnd, " -> ", showHaskell body, ")"]
showHaskell (App lt rt) = mconcat ["(", showHaskell lt, " ", showHaskell rt, ")"]
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
    apply <- parseApp

    return $ Just $ Define (ident, apply)
-}
--parseDefine = Just . Define <$> ((,) <$> (parseIdent <* literal '=') <*> parseApp)
parseDefine = ((Just . Define) .) . (,) <$> (parseIdent <* literal '=') <*> parseApp


parseExpr = Just . Expr <$> parseApp


{-
commentLine = do
    P.string "--"
    --many (P.noneOf "\n")
    many (P.noneOf "\n")
    return Nothing
-}
--commentLine = P.string "--" *> many (P.noneOf "\n") *> return Nothing
--commentLine = (P.string "--" *> many (P.noneOf "\n")) $> Nothing
commentLine = Nothing <$ (P.string "--" *> P.many (P.noneOf "\n"))


-- emptyLine = skipSpaces *> return Nothing
emptyLine = Nothing <$ skipSpaces


parseApp = do
    term <- parseTerm

    do
        terms <- P.many1 parseTerm
        --return $ foldl (\ls x -> App ls x) term terms
        return $ foldl App term terms

        <|> do
            return term


parseTerm = parseNested <|> parseFun <|> parseVar


parseNested = literal '(' *> parseApp <* literal ')'

--parseFun = Fun <$> (literal '\\' *> parseLetter <* literal '.') <*> parseApp
{-
parseFun = do
    literal '\\'
    cs <- parseLetter
    literal '.'

    Fun [cs] <$> parseApp
-}
parseFun = do
    --literal '\\'
    P.oneOf "\\λ"
    skipSpaces

    cs <- filter (not . isSpace) <$> parseArgs
    literal '.'
    apply <- parseApp

    let (s:ss) = map (\x -> [x]) $ reverse cs

    --return $ foldl (\acc c -> Fun c acc) (Fun x apply) xs
    return $ foldl (flip Fun) (Fun s apply) ss


parseVar = Var <$> parseIdent

-- #--------------------------------------------------------------------------
-- #
-- #        execute
-- #
-- #--------------------------------------------------------------------------

pPrint = putStrLn . ppShow


debugPrint t a b = do
    debugPrint1 t a
    debugPrint2 t b


debugPrint1 t a = do
    putStrLn $ "---------- " ++ t ++ " define ----------"
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> show . snd)) a
    putStrLn ""
    traverse_ (putStrLn . ((\k v -> k ++ " = " ++ v) <$> fst <*> showLambda . snd)) a
    putStrLn ""
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> showHaskell . snd)) a
    putStrLn ""


debugPrint2 t b = do
    debugPrint21 t
    debugPrint22 t b


debugPrint21 t = do
    putStrLn $ "---------- " ++ t ++ " expr ----------"


debugPrint22 t b = do
    pPrint b
    putStrLn ""
    traverse_ (putStrLn . showLambda) b
    putStrLn ""
    traverse_ (putStrLn . showHaskell) b
    putStrLn ""



executeStmts stmts = do
    let isExpr :: Stmt -> Bool
        isExpr (Expr _) = True
        isExpr _ = False

    let exps' = filter isExpr stmts
    let defs = map (\(Define x) -> x) $ stmts \\ exps'

    let dups = map (!! 0) . filter ((> 1) . length) . group . sort $ map fst defs
    when (notNull dups) (error $ mconcat ["duplicate defines (", show dups, ")"])
    --guard (notnull dups)

    let exps = map (\(Expr x) -> x) $ exps'

    putStrLn "---------- haskell ----------"
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> show . snd)) defs
    putStrLn ""
    traverse_ print exps
    putStrLn ""
    putStrLn "---------- define ----------"
    traverse_ (putStrLn . ((\k v -> k ++ " = " ++ v) <$> fst <*> showLambda . snd)) defs
    putStrLn ""
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> showHaskell . snd)) defs
    putStrLn ""
    putStrLn "---------- expr ----------"
    pPrint exps
    putStrLn ""
    traverse_ (putStrLn . showLambda) exps
    putStrLn ""
    traverse_ (putStrLn . showHaskell) exps
    putStrLn ""

    -- extract macro

    let eDefs = map (second $ extract defs) defs
    let eExps = map (extract defs) exps

    --debugPrint "macro" eDefs eExps

    -- alpha
    --let (aDefs, aLastId) = runState (traverse (\(x, y) -> (x,) <$> alpha [] y) eDefs) 0
    --let (aExps, _) = runState (traverse (alpha []) eExps) aLastId

    let (aExps, _) = runState (traverse (alpha []) eExps) 0

    debugPrint2 "alpha" aExps

    -- beta

    debugPrint21 "beta"

    bExps <- traverse repeatWhileChanging aExps

    putStrLn ""
    putStrLn "---------- complete ----------"
    putStrLn ""
    pPrint bExps
    putStrLn ""
    traverse_ (putStrLn . showLambda) bExps
    putStrLn ""
    traverse_ (putStrLn . showHaskell) bExps



repeatWhileChanging :: Term -> IO Term
repeatWhileChanging aExp = do
    let bExp = beta aExp
    --let !_ = bExp `deepseq` ()

    if aExp == bExp then do
        return bExp

        else do
            putStrLn ""
            putStrLn . showLambda $ bExp

            repeatWhileChanging bExp


-- #--------------------------------------------------------------------------
-- #
-- #        beta
-- #
-- #--------------------------------------------------------------------------

beta :: Term -> Term

beta (App lt@(Fun bnd body) rt) = repBody bnd (beta rt) (beta body)

beta (App lt rt) = App (beta lt) (beta rt)

beta (Fun bnd body) = Fun bnd (beta body)

beta org = org


-- # replace function body

repBody :: String -> Term -> Term -> Term

repBody srch val (App lt rt) =
    App (repBody srch val lt) (repBody srch val rt)

repBody srch val (Fun bnd body) = Fun bnd (repBody srch val body)

repBody srch val (Bnd key)
    | srch == key = val

repBody _ _ org = org


-- #--------------------------------------------------------------------------
-- #
-- #        alpha
-- #
-- #--------------------------------------------------------------------------

genId :: State Int Int
genId = modify (+1) >> get

alpha :: [(String, String)] -> Term -> State Int Term

alpha db (App lt rt) = App <$> alpha db lt <*> alpha db rt

alpha db (Fun bnd body) = do
    newId <- genId

    let bnd' = bnd ++ ('{' : (show newId ++ "}"))
    let db' = (bnd, bnd'):db

    Fun bnd' <$> alpha db' body

alpha db org@(Var key) = 
    return $ case lookup key db of
                Just key' -> Bnd key'
                Nothing   -> org


-- #--------------------------------------------------------------------------
-- #
-- #        extract macro
-- #
-- #--------------------------------------------------------------------------

extract :: [(String, Term)] -> Term -> Term

extract db (App lt rt) = App (extract db lt) (extract db rt)

extract db (Fun bnd body) = Fun bnd (extract db body)

extract db org@(Var key) = do
    case lookup key db of
        Just key' -> extract db key'
        Nothing   -> org


-- #--------------------------------------------------------------------------
-- #
-- #        main
-- #
-- #--------------------------------------------------------------------------

main = do
    input <- readFile "example.lmd"

    case P.parse parser "(src)" input of
        Right stmts -> executeStmts stmts
        Left err -> putStrLn "[parse error]" >> print err


    putStrLn "done."


-- EOF
