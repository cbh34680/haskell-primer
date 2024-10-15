-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}
-- {-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE CPP #-}

-- (ghci)
-- :set -DDEBUG


import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Control.Arrow (first, second)

import Debug.Trace (trace)
import Text.Show.Pretty (ppShow)

import GHC.Generics (Generic)
import Control.DeepSeq (NFData, deepseq)

import Data.Foldable (traverse_, for_)
import Data.Function ((&))
import Data.Char (isAsciiLower, isSpace)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List ((\\), group, sort, intersperse)
import Data.List.Extra (notNull)
import Test.HUnit ((~:))

import qualified Text.Parsec as P
import qualified Test.HUnit as TU
import qualified Control.Monad.State as MS
import qualified Control.Monad.Writer as MW


data Term =
    Var String |
    Bnd String |
    Fun {mKey::String, mBbody::Term} |
    App {mLT::Term, mRT::Term}
    deriving (Eq, Show, Generic)

data Stmt =
    Define NamedTerm |
    Expr Term
    deriving (Eq, Show)


instance NFData Term

type NamedTerm = (String, Term)
type Result a = Either String a


--
showLambdaKey = takeWhile (/= '{')

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


-- #--------------------------------------------------------------------------
-- #
-- #        execute
-- #
-- #--------------------------------------------------------------------------

toExprs :: [Stmt] -> Result ([NamedTerm], [Term])
toExprs stmts = do
    let isExpr :: Stmt -> Bool
        isExpr (Expr _) = True
        isExpr _ = False

    let exprs = filter isExpr stmts
    let defs = map (\(Define x) -> x) $ stmts \\ exprs

    let dups = map (!! 0) . filter ((> 1) . length) . group . sort $ map fst defs
    when (notNull dups) (Left $ mconcat ["duplicate defines (", show dups, ")"])

    let exprs' = map (\(Expr x) -> x) exprs

    return (defs, exprs')


executeStmts :: [Stmt] -> IO ()
executeStmts stmts = do
    putStrLn $ mconcat ["# ACCEPTED ", show (length stmts), " STATEMENTS"]

    either fail (uncurry executeExprs) $ toExprs stmts


-- #define DEBUG (1)

executeExprs :: [NamedTerm] -> [Term] -> IO ()
executeExprs defs exprs = do
#if defined DEBUG
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
    pPrint exprs
    putStrLn ""
    traverse_ (putStrLn . showLambda) exprs
    putStrLn ""
    traverse_ (putStrLn . showHaskell) exprs

#else
    putStrLn $ mconcat ["MACRO: ", show (length defs)]
    putStrLn $ mconcat ["EXPRESSION: ", show (length exprs)]

#endif

    putStrLn ""

    -- マクロの展開

    let eExprs = map (extract defs) exprs

    -- α変換

    let (aExprs, _) = MS.runState (traverse (alpha []) eExprs) 0

#if defined DEBUG
    debugPrint2 "alpha" aExprs

    -- beta

    debugPrint21 "beta"
#endif

    for_ (zip [0..] aExprs) $ \(i, aExpr) -> do

        -- β簡約
        let (bExpr, bExprs) = MW.runWriter (repeatWhileChanging aExpr)

        putStrLn $ mconcat [replicate 10 '-', " EXPRESSION[", show (i + 1), "] ", replicate 10 '-']

        putStrLn "# INPUT"
        putStrLn $ mconcat ["TEXT: ", showLambda (exprs !! i)]
        putStrLn ""

        putStrLn $ mconcat ["LAMBDA: ", showLambda (aExprs !! i)]
        putStrLn ""


        putStrLn "# BETA REDUCTION"
        traverse_ (\(j, l) -> putStrLn $ mconcat ["{", show j, "} ", showLambda l]) $ zip [1..] bExprs

        putStrLn ""

        putStrLn "# OUTPUT"
        putStrLn $ mconcat ["LAMBDA: ", showLambda bExpr]

        putStrLn ""
        putStrLn $ mconcat ["Haskell: ", showHaskell bExpr]

        putStrLn ""


    --putStrLn "---------- done ----------"


repeatWhileChanging :: Term -> MW.Writer [Term] Term
repeatWhileChanging aExpr = do
    -- β簡約前後が一致するまで繰り返す

    let bExpr = beta aExpr

    if aExpr == bExpr then do
        return bExpr

        else do
            MW.tell [bExpr]

            repeatWhileChanging bExpr


#if defined DEBUG
pPrint :: Show a => a -> IO ()
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
#endif


-- #--------------------------------------------------------------------------
-- #
-- #        beta reduction
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
-- #        alpha conversion
-- #
-- #--------------------------------------------------------------------------

genId :: MS.State Int Int
genId = MS.modify (+1) >> MS.get

alpha :: [(String, String)] -> Term -> MS.State Int Term

alpha db (App lt rt) = App <$> alpha db lt <*> alpha db rt

alpha db (Fun bnd body) = do
    -- ラムダ抽象の引数 (ex. "f") を一意の名前 (ex. "f{15}") に置き換える
    newId <- genId

    let bnd' = bnd ++ ('{' : (show newId ++ "}"))
    let db' = (bnd, bnd'):db

    Fun bnd' <$> alpha db' body

alpha db org@(Var key) = return . maybe org Bnd $ lookup key db


-- #--------------------------------------------------------------------------
-- #
-- #        extract macro
-- #
-- #--------------------------------------------------------------------------

extract :: [(String, Term)] -> Term -> Term

extract db (App lt rt) = App (extract db lt) (extract db rt)

extract db (Fun bnd body) = Fun bnd (extract db body)

extract db org@(Var key) = maybe org (extract db) $ lookup key db


-- #--------------------------------------------------------------------------
-- #
-- #        parse text
-- #
-- #--------------------------------------------------------------------------

separator = P.oneOf " \t"
skipSpaces = void (P.skipMany separator)

parseIdent = ((:) <$> P.letter <*> P.many (P.letter <|> P.digit)) <* skipSpaces

parseArgs = P.many1 (P.satisfy isAsciiLower <|> P.char ' ') <* skipSpaces

eol = P.char '\n'

literal c = P.char c <* skipSpaces

parser = (catMaybes <$> P.many parseStmt) <* P.eof

parseStmt = skipSpaces *>
    (P.try parseDefine <|> commentLine <|> parseExpr <|> emptyLine) <* eol

parseDefine = ((Just . Define) .) . (,) <$> (parseIdent <* literal '=') <*> parseApp

parseExpr = Just . Expr <$> parseApp

commentLine = Nothing <$ (P.string "--" *> P.many (P.noneOf "\n"))

emptyLine = Nothing <$ skipSpaces

parseApp = do
    term <- parseTerm
    foldl App term <$> P.many1 parseTerm <|> return term

parseTerm = parseNested <|> parseFun <|> parseVar

parseNested = literal '(' *> parseApp <* literal ')'

parseFun = do
    P.oneOf "\\λ"
    skipSpaces

    cs <- filter (not . isSpace) <$> parseArgs
    literal '.'
    apply <- parseApp

    let (s:ss) = map (:[]) $ reverse cs

    return $ foldl (flip Fun) (Fun s apply) ss

parseVar = Var <$> parseIdent


-- #--------------------------------------------------------------------------
-- #
-- #        main
-- #
-- #--------------------------------------------------------------------------

main = do
    input <- readFile "example.lmd"

    case P.parse parser "(src)" input of
        Right stmts -> do
            executeStmts stmts

        Left err -> putStrLn "[parse error]" >> print err

    putStrLn "# ALL DONE"


-- #--------------------------------------------------------------------------
-- #
-- #        tests
-- #
-- #--------------------------------------------------------------------------

evalWriter m = fst (MW.runWriter m)

testMacros = [
    "c0 = (λf.(λx.x))",
    "c1 = (succ c0)",
    "c2 = (succ c1)",
    "c3 = (succ c2)",
    "c4 = (succ c3)",
    "c5 = (succ c4)",
    "c6 = (succ c5)",
    "c7 = (succ c6)",
    "c8 = (succ c7)",
    "c9 = (succ c8)",
    "succ = (λn.(λf.(λx.(f ((n f) x)))))",
    "pred = (λn.(λf.(λx.(((n (λg.(λh.(h (g f))))) (λu.x)) (λu.u)))))",
    "const = (λa.(λb.a))",
    "id = (λa.a)",
    "true = (λx.(λy.x))",
    "false = (λx.(λy.y))",
    "and = (λp.(λq.((p q) false)))",
    "or = (λp.(λq.((p true) q)))",
    "not = (λp.((p false) true))",
    "if = (λp.(λx.(λy.((p x) y))))",
    "iszero = (λn.((n (λx.false)) true))",
    "plus = (λm.(λn.((m succ) n)))",
    "mult = (λm.(λn.(λf.(n (m f)))))",
    "pair = (λf.(λs.(λb.((b f) s))))",
    "first = (λp.(p true))",
    "secnd = (λp.(p false))",
    "nil = (λc.(λn.n))",
    "cons = (λx.(λl.(λc.(λn.((c x) ((l c) n))))))",
    ""                                                      -- need!
    ]

evalOne :: String -> Term

evalOne stmt = do
    let input = mconcat $ intersperse "\n" (stmt:testMacros)
    let (Right stmts) = P.parse parser "(test1)" input
    let (Right (defs, exprs)) = toExprs stmts
    let eExprs = map (extract defs) exprs
    let aExprs = MS.evalState (traverse (alpha []) eExprs) 0
    let bExprs = map (evalWriter . repeatWhileChanging) aExprs

    head bExprs


expectEqual stmt expected = expected TU.~=? (showLambda . evalOne $ stmt)


t :: IO ()
t = do
    putStrLn ""
    putStrLn ">> EXECUTE TEST"
    putStrLn ""

    TU.runTestTT $ TU.TestList [
        --
        -- title                    statement/expected
        --
         "test-1"   ~: expectEqual  "c3"
                                    "(λf.(λx.(f (f (f x)))))"

        ,"test-2"   ~: expectEqual  "pred c3"
                                    "(λf.(λx.(f (f x))))"

        ,"test-3"   ~: expectEqual  "plus c4 c5"
                                    "(λf.(λx.(f (f (f (f (f (f (f (f (f x)))))))))))"

        ,"test-4"   ~: expectEqual  "const c1 c2"
                                    "(λf.(λx.(f x)))"

        ,"test-5"   ~: expectEqual  "const id c1 c2"
                                    "(λf.(λx.(f (f x))))"

        ,"test-6"   ~: expectEqual  "if (iszero (pred c2)) (plus c7 c8) (c4)"
                                    "(λf.(λx.(f (f (f (f x))))))"

        ,"test-7"   ~: expectEqual  "if (iszero (pred c1)) (plus c7 c8) (c4)"
                                    "(λf.(λx.(f (f (f (f (f (f (f (f (f (f (f (f (f (f (f x)))))))))))))))))"

        ,"test-8"   ~: expectEqual  "mult c2 c3"
                                    "(λf.(λx.(f (f (f (f (f (f x))))))))"

        ,"test-9"   ~: expectEqual  "mult (pred c3) (plus c3 (plus c1 c2))"
                                    "(λf.(λx.(f (f (f (f (f (f (f (f (f (f (f (f x))))))))))))))"

        ,"test-10"  ~: expectEqual  "plus c3 (plus c1 c2)"
                                    "(λf.(λx.(f (f (f (f (f (f x))))))))"

        ,"test-11"  ~: expectEqual  "cons c1 (cons c2 (cons c3 nil))"
                                    "(λc.(λn.((c (λf.(λx.(f x)))) ((c (λf.(λx.(f (f x))))) ((c (λf.(λx.(f (f (f x)))))) n)))))"

        ,"test-12"  ~: expectEqual  "and true false"
                                    "(λx.(λy.y))"

        ,"test-13"  ~: expectEqual  "and true true"
                                    "(λx.(λy.x))"

        ,"test-14"  ~: expectEqual  "or true false"
                                    "(λx.(λy.x))"

        ,"test-15"  ~: expectEqual  "or true true"
                                    "(λx.(λy.x))"

        ,"test-16"  ~: expectEqual  "first (pair c5 c6)"
                                    "(λf.(λx.(f (f (f (f (f x)))))))"

        ,"test-17"  ~: expectEqual  "secnd (pair c5 c6)"
                                    "(λf.(λx.(f (f (f (f (f (f x))))))))"






        ,"test-99"  ~: expectEqual  "(λx.(x x)) (λx.(x x))"
                                    "((λx.(x x)) (λx.(x x)))"
        ]

    putStrLn ""


-- EOF
