-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

-- :set -DNOT_DIVE

import Control.Applicative ((<|>))
import Control.Monad (void, when)
import Control.Arrow (first, second)
import Control.Monad.State

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
    Fun {mBbound::String, mBbody::Term} |
    App {mLeft::Term, mRight::Term}
    deriving (Eq, Show, Generic)

instance NFData Term

data Stmt = Define (String, Term) | Expr Term deriving (Eq, Show)

type Indent = Int

showLambdaKey = takeWhile (/= '#')

showLambda (Fun key term) = mconcat ["(λ", showLambdaKey key, ".", showLambda term, ")"]
showLambda (App lt rt) = mconcat ["(", showLambda lt, " ", showLambda rt, ")"]
showLambda (Var key) = showLambdaKey key

showType (Var _) = "V"
showType (Fun _ _) = "L"
showType (App _ _) = "A"

showHaskell (Fun key term) = mconcat ["(\\", showLambdaKey key, " -> ", showHaskell term, ")"]
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
    putStrLn $ "---------- " ++ t ++ " expr ----------"
    pPrint $ b
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
    when (notNull dups) (error $ mconcat ["duplicate terms (", show dups, ")"])
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
    pPrint $ exps
    putStrLn ""
    traverse_ (putStrLn . showLambda) exps
    putStrLn ""
    traverse_ (putStrLn . showHaskell) exps
    putStrLn ""

    putStrLn "---------- macro define ----------"

    -- extract macro

    let eDefs = map (second $ extract defs) defs
    let eExps = map (extract defs) exps

    debugPrint "macro" eDefs eExps

    --let eDefs = defs
    --let eExps = exps

    -- alpha
    let (aDefs, aLastId) = runState (traverse (\(x, y) -> (x,) <$> alpha [] y) eDefs) 0
    let (aExps, _) = runState (traverse (alpha []) eExps) aLastId

    debugPrint "alpha" aDefs aExps

    -- beta

    let bExps = map (\x -> evalState (beta [] x) []) aExps

    putStrLn "==="
    let !_ = bExps `deepseq` ()
    putStrLn ""

    debugPrint2 "beta" bExps

    --putStrLn ""
    --putStrLn "---------- complete ----------"
    --putStrLn ""


-- #--------------------------------------------------------------------------
-- #
-- #        beta
-- #
-- #--------------------------------------------------------------------------


callable :: Term -> Bool
callable (Var _) = False
callable _ = True


argsPush :: Term -> State [Term] ()

argsPush x = do
    xs <- get
    put (x:xs)


argsPop :: State [Term] (Maybe Term)

argsPop = do
    xs <- get

    if null xs then return Nothing else do
        let (x':xs') = xs
        put xs'

        return $ Just x'


argsIsEmpty :: State [Term] Bool

argsIsEmpty = do
    xs <- get
    return $ null xs


-- #--------

beta :: [(String, Term)] -> Term -> State [Term] Term

beta db (App lt rt) = do
    --let !_ = trace (mkdbg 0 "App" [ "<" ,showLambda lt, showLambda rt ]) 1

    -- 引数スタックに追加
    argsPush rt

    -- 左側を評価
    lt' <- beta db lt

    isEmpty <- argsIsEmpty

    if isEmpty then
        {-
            "引数スタックが空 (消費された)" なら (関数が実行された状態なので) 
            戻り値をそのまま返却
        -}

        return lt'

        else do
            {-
                引数が消費されていないときは、右側を評価
            -}

            -- 引数スタックはリセット
            chkRt <- argsPop

            -- 念のためチェック
            when (isJust chkRt) (when (fromJust chkRt /= rt) (error "rt != chkRt"))

            -- 右側を評価して再設定

            rt' <- beta db rt

            return $ App lt' rt'


beta db org@(Fun key term) = do
    let !_ = trace (mkdbg 0 "Fun" [ "<" ,key, showLambda term ]) 1

    -- 引数スタックから取り出し
    arg <- argsPop

    case arg of
        Just arg' -> do
            -- 取り出し成功 (引数があった)

            -- 変数マップに、仮引数をキーとして登録
            let db' = (key, arg'):db

            -- 本体を評価
            beta db' term


        Nothing -> do
            -- 引数がないときは、本体を評価したものを再設定

            term' <- beta db term

            return $ Fun key term'


beta db org@(Var key) = do
    --let !_ = trace (mkdbg 0 "Var" [ "<" ,key ]) 1

    -- キー名を元に変数マップをたどる

    case lookup key db of
        --Just x -> beta db x

        Just x -> if x /= org then beta db x
                    else do
                        -- 無限ループ回避
                        let !_ = trace (mconcat $ intersperse "|" [replicate 30 ' ', "* equal ref", showLambda org, show org ]) 1

                        return org


        _ -> return org



-- #--------------------------------------------------------------------------
-- #
-- #        alpha
-- #
-- #--------------------------------------------------------------------------

genId :: State Int Int
genId = modify (+1) >> get

alpha :: [(String, String)] -> Term -> State Int Term

alpha db (App lt rt) = do
    lt' <- alpha db lt
    rt' <- alpha db rt

    return $ App lt' rt'


alpha db (Fun key term) = do
    newId <- genId

    let key' = key ++ ('#' : (show newId))
    let db' = (key, key'):db

    term' <- alpha db' term

    return $ Fun key' term'
    

alpha db org@(Var key) = do
    case lookup key db of
        Just key' -> return $ Var key'
        Nothing   -> return org


-- #--------------------------------------------------------------------------
-- #
-- #        extract macro
-- #
-- #--------------------------------------------------------------------------

extract :: [(String, Term)] -> Term -> Term

extract db (App lt rt) = do
    let lt' = extract db lt
    let rt' = extract db rt

    App lt' rt'


extract db (Fun key term) = do
    let term' = extract db term

    Fun key term'


extract db org@(Var key) = do
    case lookup key db of
        Just key' -> extract db key'
        Nothing   -> org


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
