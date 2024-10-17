{-# LANGUAGE CPP #-}

-- (ghci)
-- :set -DDEBUG

-- #define DEBUG (1)
#define TEST (1)

import System.IO
import Control.Applicative ((<|>))
import Control.Monad (void, when, (<=<))
--import Control.Monad.Extra (whenJust)
import Control.Arrow (first, second)

--import GHC.Generics (Generic)
--import Control.DeepSeq (NFData, deepseq)

import Data.Foldable (traverse_, for_)
import Data.Function ((&))
import Data.Char (isAsciiLower, isSpace)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List ((\\), group, sort, intersperse)
import Data.List.Extra (notNull)
import Debug.Trace (trace)

import qualified Text.Parsec as P
import qualified Control.Monad.State as MS
import qualified Control.Monad.Writer as MW

#if defined DEBUG
import Text.Show.Pretty (ppShow)
#endif

#if defined TEST
import Test.HUnit ((~:))

import qualified Test.HUnit as TU
#endif


data Term =
    Var String |
    Bnd String |
    Fun {mKey::String, mBbody::Term} |
    App {mLT::Term, mRT::Term}
    deriving (Eq, Show)
    --deriving (Eq, Show, Generic)

--instance NFData Term

data Stmt =
    Define NamedTerm |
    Expr Term
    deriving (Eq, Show)

type NamedTerm = (String, Term)
type Result a = Either String a

--
showLambdaKey = takeWhile (/= '{')

showLambda (Fun bnd body) = mconcat ["(λ", showLambdaKey bnd, ".", showLambda body, ")"]
showLambda (App lt rt) = mconcat ["(", showLambda lt, " ", showLambda rt, ")"]
showLambda (Var key) = showLambdaKey key
showLambda (Bnd key) = showLambdaKey key

--
showType (Var _) = "V"
showType (Bnd _) = "B"
showType (Fun _ _) = "F"
showType (App _ _) = "A"

--
showHaskell (Fun bnd body) = mconcat ["(\\", showLambdaKey bnd, " -> ", showHaskell body, ")"]
showHaskell (App lt rt) = mconcat ["(", showHaskell lt, " ", showHaskell rt, ")"]
showHaskell x = showLambda x

#if defined DEBUG
showLambdaAKey = id

showLambdaA (Fun bnd body) = mconcat ["(λ", showLambdaAKey bnd, ".", showLambdaA body, ")"]
showLambdaA (App lt rt) = mconcat ["(", showLambdaA lt, " ", showLambdaA rt, ")"]
showLambdaA (Var key) = showLambdaAKey key
showLambdaA (Bnd key) = showLambdaAKey key
#endif


-- #--------------------------------------------------------------------------
-- #
-- #        execute
-- #
-- #--------------------------------------------------------------------------

{-
    Parsec で構築した Stmt をマクロ定義 (NamedTerm) と実行式 (Term) に分ける

    NAME = EXPR ... マクロ定義
        ex) "succ = (λn.(λf.(λx.(f ((n f) x)))))"

    EXPR ... 実行式
        ex) "plus c2 c3"
-}
toExprs :: [Stmt] -> Result ([NamedTerm], [Term])
toExprs stmts = do
    let isExpr :: Stmt -> Bool
        isExpr (Expr _) = True
        isExpr _ = False

    let exprs = filter isExpr stmts
    let defs = map (\(Define x) -> x) $ stmts \\ exprs

    -- マクロ定義名の重複チェック
    let dups = map (!! 0) . filter ((> 1) . length) . group . sort $ map fst defs
    when (notNull dups) (Left $ mconcat ["duplicate defines (", show dups, ")"])

    let exprs' = map (\(Expr x) -> x) exprs

    return (defs, exprs')


-- Stmt の分類が成功したら式を実行する
executeStmts :: [Stmt] -> IO ()
executeStmts stmts = do
    putStrLn $ mconcat ["# ACCEPTED ", show (length stmts), " STATEMENTS"]

    either fail (uncurry evalExprs) $ toExprs stmts


{-
    式の実行
        1) マクロの展開
        2) α変換
        3) β簡約
        4) 表示
-}
evalExprs :: [NamedTerm] -> [Term] -> IO ()
evalExprs defs exprs = do
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

    -- [マクロの展開]
    -- "c1" を (\f.(\x.f(x)) に置換
    let eExprs = map (extract defs) exprs

    -- [α変換]
    -- (\f.(\x.(f(x)))) を (\f{1}.(\x{2}.f{1}(x{2}))) に書き換え
    let (aExprs, _) = MS.runState (traverse (alpha []) eExprs) 0

#if defined DEBUG
    debugPrint2 "alpha" aExprs

    -- beta

    debugPrint21 "beta"
#endif

    {- [β簡約]
       (\f{1}.(\x{2}.f{1}(x{2}))) A B のとき

        --> (\f{1}.(\x{2}.f{1}(x{2}))) A B
        --> (A.(\x{2}.A(x{2}))) B
        --> (\x{2}.A(x{2})) B
        --> (B.A(B))
        --> A(B)

        のように引数を関数に適用していく
    -}
    for_ (zip [0..] aExprs) $ \(i, aExpr) -> do
        -- (最終形, [途中経過])
        let (bExpr, bExprs) = MW.runWriter (repeatWhileChanging aExpr)

        putStrLn $ mconcat [replicate 10 '-', " EXPRESSION[", show (i + 1), "] ", replicate 10 '-']

        putStrLn "# INPUT"
        putStrLn $ mconcat ["TEXT: ", showLambda (exprs !! i)]
        putStrLn ""

        putStrLn $ mconcat ["LAMBDA: ", showLambda (aExprs !! i)]
        putStrLn ""

#if defined DEBUG
        putStrLn $ mconcat ["LAMBDA(α): ", showLambdaA (aExprs !! i)]
        putStrLn ""
#endif

        putStrLn "# BETA REDUCTION"
        traverse_ (\(j, l) -> putStrLn $ mconcat ["{", show j, "} ", showLambda l]) $ zip [1..] bExprs

        putStrLn ""

        putStrLn "# OUTPUT"
        putStrLn $ mconcat ["LAMBDA: ", showLambda bExpr]
        putStrLn ""

#if defined DEBUG
        putStrLn $ mconcat ["LAMBDA(α): ", showLambdaA bExpr]
        putStrLn ""
#endif

        putStrLn $ mconcat ["Haskell: ", showHaskell bExpr]
        putStrLn ""

        putStrLn . mconcat . intersperse "\n" . catMaybes $ sequenceA
            [withT "Church Numericals" <=< toChurchNum
            ,withT "Church Booleans"   <=< toChurchBool] bExpr

        putStrLn ""


-- 出力補助
withT :: (Monad m, Show a) => String -> a -> m String
withT t x = return $ mconcat [t, ": ", show x]


-- チャーチ真理値に変換
toChurchBool :: Term -> Maybe Bool

toChurchBool (Fun bndx (Fun bndy (Bnd v)))
    | bndx == v = return True
    | bndy == v = return False

toChurchBool _ = Nothing


-- チャーチ数に変換
toChurchNum :: Term -> Maybe Int

toChurchNum (Fun _ (Fun bndx (Bnd x)))
    | bndx == x = return 0

toChurchNum (Fun bndf (Fun bndx app@(App _ _))) = cntBndf bndf bndx app

toChurchNum _ = Nothing


cntBndf :: String -> String -> Term -> Maybe Int

cntBndf bndf bndx (App (Bnd f) app@(App _ _))
    | bndf == f = (+1) <$> cntBndf bndf bndx app

cntBndf bndf bndx (App (Bnd f) (Bnd x))
    | bndf == f && bndx == x = return 1

cntBndf _ _ org = Nothing


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

{-
    引数がある関数適用 ((\x. M) N) の場合は関数本体 M 中の x を N に置き換えし
    引数のない関数の場合は、本体 M を再帰的に簡約する
-}
beta :: Term -> Term

-- (\x. M) N の場合は M 中に含まれる x (ex. Bnd "f{15}") を N に置き換え
beta (App lt@(Fun bnd body) rt) = repBody bnd (beta rt) (beta body)

-- (M N) の場合は両方を簡約
beta (App lt rt) = App (beta lt) (beta rt)

-- (\x. M) の場合は M を簡約
beta (Fun bnd body) = Fun bnd (beta body)

-- それ以外は変化なし
beta org = org


-- 関数本体中の Bnd "f{15}" を再帰的に val で置き換え
repBody :: String -> Term -> Term -> Term

repBody srch val (App lt rt) =
    App (repBody srch val lt) (repBody srch val rt)

repBody srch val (Fun bnd body) = Fun bnd (repBody srch val body)

repBody srch val (Bnd key)
    | srch == key = val

repBody _ _ org = org


-- 簡約の前後が一致するまで繰り返す
repeatWhileChanging :: Term -> MW.Writer [Term] Term

repeatWhileChanging aExpr = do
    let bExpr = beta aExpr

    if aExpr == bExpr then do
        return bExpr

        else do
            MW.tell [bExpr]

            repeatWhileChanging bExpr

-- #--------------------------------------------------------------------------
-- #
-- #        alpha conversion
-- #
-- #--------------------------------------------------------------------------

-- 関数の引数 (ex. "f") を一意の名前 (ex. "f{15}") に置換
alpha :: [(String, String)] -> Term -> MS.State Int Term

alpha db (App lt rt) = App <$> alpha db lt <*> alpha db rt

alpha db (Fun bnd body) = do
    newId <- genId

    let bnd' = bnd ++ ('{' : (show newId ++ "}"))
    let db' = (bnd, bnd'):db

    -- 関数本体中の Var "f" を Bnd "f{15}" に(再帰的)置換
    Fun bnd' <$> alpha db' body

alpha db org@(Var key) = return . maybe org Bnd $ lookup key db

--
genId :: MS.State Int Int
genId = MS.modify (+1) >> MS.get

-- #--------------------------------------------------------------------------
-- #
-- #        extract macro
-- #
-- #--------------------------------------------------------------------------

-- plus, const などのマクロ定義を実際の関数に置換
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

eol = P.char '\n'

literal c = P.char c <* skipSpaces

--
parser = (catMaybes <$> P.many parseStmt) <* P.eof

parseStmt = skipSpaces *>
    (P.try parseDefine <|> parseComment <|> parseExpr <|> parseEmpty) <* eol

{-
parseDefine = do
    ident <- parseIdent
    literal '='
    apply <- parseApp

    return $ Just $ Define (ident, apply)
-}
parseDefine = Just . Define <$> ((,) <$> (parseIdent <* literal '=') <*> parseApp)
--parseDefine = ((Just . Define) .) . (,) <$> (parseIdent <* literal '=') <*> parseApp

parseExpr = Just . Expr <$> parseApp

parseComment = Nothing <$ (P.string "--" *> P.many (P.noneOf "\n"))

parseEmpty = Nothing <$ skipSpaces

--
parseApp = do
    term <- parseTerm
    foldl App term <$> P.many1 parseTerm <|> return term

parseTerm = parseNested <|> parseFun <|> parseVar <|> parseNumber

parseNested = literal '(' *> parseApp <* literal ')'

parseFun = do
    P.oneOf "\\λ"
    skipSpaces

    cs <- filter (not . isSpace) <$> parseArgs
    literal '.'
    apply <- parseApp

    let (s:ss) = map (:[]) $ reverse cs

    return $ foldl (flip Fun) (Fun s apply) ss

--parseVar :: P.Parsec String () Term
parseVar = Var <$> parseIdent

parseNumber = do
    -- 数値のみの場合はチャーチ数に変換する
    cs <- P.many1 P.digit
    skipSpaces

    let n = (read :: (String -> Int)) cs
    let body = foldr App (Var "x") $ replicate n (Var "f")

    return $ Fun "f" (Fun "x" body)

--
parseIdent = ((:) <$> P.letter <*> P.many (P.letter <|> P.digit)) <* skipSpaces

parseArgs = P.many1 (P.satisfy isAsciiLower <|> P.char ' ') <* skipSpaces


-- #--------------------------------------------------------------------------
-- #
-- #        main
-- #
-- #--------------------------------------------------------------------------

main :: IO ()
main = do
    input <- readFile "example.lmd"

    case P.parse parser "(src)" input of
        Right stmts -> do
            executeStmts stmts

        Left err -> putStrLn "[parse error]" >> print err

    putStrLn "# ALL DONE"


#if defined TEST
-- #--------------------------------------------------------------------------
-- #
-- #        tests
-- #
-- #--------------------------------------------------------------------------

{-
    ghci>
        :l lambda.hs    このファイルを読み込み
        t               テストを実行
        tw              "example.lmd" を生成 
        :main           "example.lmd" の内容を評価
-}
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
    "Y = (λf. (λx. f(x x)) (λx. f(x x)))",
    "",
    "-- alias",
    "car = true",
    "cdr = false",


    "" -- LAST: need!
    ]

tw :: IO ()
tw = do
    let outs = testMacros ++ ["plus c2 3", "const id 8 c9", "iszero (pred 1)"]

    {-
    withFile "example.lmd" WriteMode $
        \h -> hPutStrLn h . mconcat $ intersperse "\n" outs
    -}
    writeFile "example.lmd" (mconcat (intersperse "\n" outs) ++ "\n")

    putStrLn $ mconcat [show (length outs), " lines were output."]

evalWriter :: MW.Writer w a -> a
evalWriter m = fst (MW.runWriter m)

{-
    ghci> eval "pred c2" 
-}
eval :: String -> IO ()
eval cs = putStrLn . showLambda $ eval1 cs

eval1 :: String -> Term
eval1 cs = do
    let input = mconcat $ intersperse "\n" (cs:testMacros)
    let (Right stmts) = P.parse parser "(src)" input
    let (Right (defs, exprs)) = toExprs stmts
    let eExprs = map (extract defs) exprs
    let aExprs = MS.evalState (traverse (alpha []) eExprs) 0
    let bExprs = map (evalWriter . repeatWhileChanging) aExprs

    head bExprs

expectEqual :: String -> String -> TU.Test
expectEqual actual expected = expected TU.~=? (showLambda . eval1 $ actual)

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

        ,"test-18"  ~: expectEqual  "pair (pair a b) c car car"
                                    "a"

        ,"test-19"  ~: expectEqual  "plus 3 3"
                                    "(λf.(λx.(f (f (f (f (f (f x))))))))"




        ,"test-99"  ~: expectEqual  "(λx.(x x)) (λx.(x x))"
                                    "((λx.(x x)) (λx.(x x)))"
        ]

    putStrLn ""

#endif


-- EOF
