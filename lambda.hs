-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}
{-# LANGUAGE CPP #-}

-- :set -DNOT_DIVE

import Debug.Trace (trace)
--import Text.Pretty.Simple (pPrint)
import Text.Show.Pretty (ppShow)

--
import Control.Applicative
import Control.Monad
import Control.Monad.State as MS

import GHC.Generics (Generic)
import Control.DeepSeq

import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.Char (isAsciiLower, isSpace)
import Data.Maybe (catMaybes, isJust, fromJust)
import Data.List ((\\), group, sort, intersperse)
import Data.List.Extra (notNull)
--import Data.List.Split (splitOn)

import qualified Text.Parsec as P


data PTerm =
    PApply {pFunc'::PTerm, pAarg'::PTerm} |
    PFunction {pBound'::Char, pBody'::PTerm} |
    PVar String
    deriving (Eq, Show, Generic)

newtype Bound = Bound String deriving (Eq, Show, Generic)

data ETerm =
    EApply {eFunc'::ETerm, eAarg'::ETerm} |
    EFunction {eBound'::Bound, eBody'::ETerm} |
    EBound Bound |
    ELabel String
    deriving (Eq, Show, Generic)

instance NFData PTerm
instance NFData ETerm
instance NFData Bound

data Stmt = Define (String, PTerm) | Expr PTerm deriving (Eq, Show)
type Indent = Int


-- #--------------------------------------------------------------------------
-- #
-- #        u t i l
-- #
-- #--------------------------------------------------------------------------

showPLambda (PApply t1 t2) = mconcat ["(", showPLambda t1, " ", showPLambda t2, ")"]
showPLambda (PFunction c term) = mconcat ["(λ", [c], ".", showPLambda term, ")"]
showPLambda (PVar cs) = cs

showPType (PApply _ _) = "A"
showPType (PFunction _ _) = "F"
showPType (PVar _) = "V"

showPHaskell (PApply t1 t2) = mconcat ["(", showPHaskell t1, " ", showPHaskell t2, ")"]
showPHaskell (PFunction c term) = mconcat ["(\\", [c], " -> ", showPHaskell term, ")"]
showPHaskell x = showPLambda x

--
showELambda (EFunction (Bound cs) term) = mconcat ["(λ", boundName cs, ".", showELambda term, ")"]
showELambda (EApply t1 t2) = mconcat ["(", showELambda t1, " ", showELambda t2, ")"]
showELambda (EBound (Bound cs)) = boundName cs
showELambda (ELabel cs) = cs

showEType (EApply _ _) = "A"
showEType (EFunction _ _) = "F"
showEType (EBound _) = "B"
showEType (ELabel _) = "L"

showEHaskell (EFunction (Bound cs) term) = mconcat ["(\\", boundName cs, " -> ", showEHaskell term, ")"]
showEHaskell (EApply t1 t2) = mconcat ["(", showEHaskell t1, " ", showEHaskell t2, ")"]
showEHaskell x = showELambda x

--boundName = fst . break (== '#')
boundName = takeWhile (/= '#')

--mkdbg lv t xs = (indentStr lv) ++ (mconcat . (intersperse "|") $ ["|", t] ++ xs ++ ["|"])

mkdbg lv t = (++) (indentStr lv) . mconcat . intersperse "|" . (++) ["|", t] . flip (++) ["|"]

indentStr :: Indent -> String
indentStr n = replicate (n * 2) ' '


-- #--------------------------------------------------------------------------
-- #
-- #        p a r s e
-- #
-- #--------------------------------------------------------------------------
--
eol = P.char '\n'

separator = P.oneOf " \t"

skipSpaces = void (P.skipMany separator)

literal c = P.char c <* skipSpaces

--
parser = (catMaybes <$> P.many parseStmt) <* P.eof

parseStmt = skipSpaces *>
    (P.try parseDefine <|> parseComment <|> parseExpr <|> parseEmpty) <* eol

--
parseDefine = ((Just . Define) .) . (,) <$> (parseIdent <* literal '=') <*> parseTermOrApply

parseComment = Nothing <$ (P.string "--" *> many (P.noneOf "\n"))

parseExpr = Just . Expr <$> parseTermOrApply

parseEmpty = Nothing <$ skipSpaces

--
parseIdent = ((:) <$> P.letter <*> P.many (P.letter <|> P.digit)) <* skipSpaces

parseArgs = P.many1 (P.satisfy isAsciiLower <|> P.char ' ') <* skipSpaces

--
parseTermOrApply = do
    term <- parseTerm

    do
        terms <- P.many1 parseTerm

        return $ foldl PApply term terms

        <|> do
            return term


parseTerm = parseNested <|> parseFunction <|> parseVar


--
parseNested = literal '(' *> parseTermOrApply <* literal ')'

parseFunction = do
    --literal '\\'
    --P.string "\\" <|> P.string "λ"
    P.oneOf "\\λ" <* skipSpaces

    cs <- filter (not . isSpace) <$> parseArgs
    literal '.'
    apply <- parseTermOrApply

    let (x:xs) = reverse cs

    --return $ foldl (\acc c -> Function c acc) (Function x apply) xs
    return $ foldl (flip PFunction) (PFunction x apply) xs


parseVar = PVar <$> parseIdent


-- #--------------------------------------------------------------------------
-- #
-- #        e x e c u t e
-- #
-- #--------------------------------------------------------------------------

executeStmts stmts = do
    let isExpr :: Stmt -> Bool
        isExpr (Expr _) = True
        isExpr _ = False

    -- 入力データのうち、広域定義ではないもの ... 評価対象
    let exprs = filter isExpr stmts

    -- 広域定義 "^a = b"
    let defs = map (\(Define t) -> t) $ stmts \\ exprs

    -- 重複する広域定義名は NG
    let dups = map (!! 0) . filter ((> 1) . length) . group . sort $ map fst defs
    when (notNull dups) (error (mconcat ["duplicate terms (", show dups, ")"]))

    let exprs' = map (\(Expr x) -> x) exprs

    putStrLn "---------- lambda ----------"
    traverse_ (putStrLn . showPLambda) exprs'
    putStrLn "---------- haskell ----------"
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> show . snd)) defs
    putStrLn ""
    traverse_ print exprs'
    putStrLn ""
    putStrLn "---------- define ----------"
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> showPLambda . snd)) defs
    putStrLn ""
    putStrLn "---------- expr ----------"
    putStrLn . ppShow $ exprs'
    putStrLn ""

    putStrLn "---------- alpha ----------"


    --let extExprs = map (\(Expr x) -> evalState (alpha 0 defs [] x) 0) exprs
    let aExprs = map (\x -> evalState (alpha 0 defs [] x) 0) exprs'
    let !_ = aExprs `deepseq` ()

    traverse_ (putStrLn . ppShow) $ zip [1..] aExprs

    putStrLn ""
    traverse_ (putStrLn . showELambda) aExprs
    putStrLn ""
    traverse_ (putStrLn . showEHaskell) aExprs
    putStrLn ""

    putStrLn "---------- beta ----------"

    let bExprs = map (beta 0 [] []) aExprs
    let !_ = bExprs `deepseq` ()

    traverse_ (putStrLn . ppShow) bExprs

    putStrLn ""
    traverse_ (putStrLn . showELambda) bExprs
    putStrLn ""
    traverse_ (putStrLn . showEHaskell) bExprs
    putStrLn ""

    putStrLn "---------- complete ----------"
    putStrLn "[INPUT]"
    putStrLn "* lambda"
    traverse_ (putStrLn . showPLambda) exprs'
    putStrLn "* haskell"
    traverse_ (putStrLn . showPHaskell) exprs'
    putStrLn ""
    putStrLn "[OUTPUT]"
    putStrLn "* lambda"
    traverse_ (putStrLn . showELambda) bExprs
    putStrLn "* haskell"
    traverse_ (putStrLn . showEHaskell) bExprs
    putStrLn ""
    putStrLn "[TEST]"
    traverse_ (\x -> putStrLn $ showEHaskell x ++ " (+1) 0") bExprs
    putStrLn ""


-- #--------------------------------------------------------------------------
-- #
-- #        b e t a   r e d u c t i o n
-- #
-- #--------------------------------------------------------------------------

beta :: Indent -> [ETerm] -> [(Bound, ETerm)] -> ETerm -> ETerm

beta lv args db (EApply lt rt) = do
    --let !_ = trace (mkdbg lv "App" [ "<" ,showEType lt ++ ":" ++ showELambda lt ,showEType rt ++ ":" ++ showELambda rt ]) 1

    -- 右を引数として登録
    let args' = rt:args

    -- 左を簡約
    beta (lv + 1) args' db lt


beta lv args db (EFunction eb term) = do
    --let !_ = trace (mkdbg lv "Fun" [ "<" ,show eb ,showEType term ++ ":" ++ showELambda term ]) 1

    if null args then do
        --let !_ = trace (mconcat [replicate 40 ' ', "no args"]) 1
        --let !_ = trace (mconcat [replicate 40 ' ', "* db: ", ppShow db]) 1

        -- 残りを簡約
        let term' = betaR (lv + 1) db term

        EFunction eb term'

        else do
            -- 引数を消費
            let (arg':args') = args

            -- 仮引数マップに登録
                db' = (eb, arg'):db

            -- 本体を簡約
            beta (lv + 1) args' db' term


beta lv args db (EBound eb) = do
    --let !_ = trace (mkdbg lv "Bou" [ ">" ,show eb ]) 1

    -- 仮引数マップを参照
    case lookup eb db of
        Just x -> beta (lv + 1) args db x
        _ -> error $ mconcat ["Not Founc:", show eb]


beta lv args db org@(ELabel key) = do
    --let !_ = trace (mkdbg lv "Lab" [ ">" ,show org ]) 1

    let !_ = error "Invalid Term Type !!"

    org


--
-- 引数が全て消費された状況で、関数本体にある参照を簡約
--

betaR :: Indent -> [(Bound, ETerm)] -> ETerm -> ETerm

betaR lv db (EApply lt rt) = do
    --let !_ = trace (mkdbg lv "App" [ "<" ,showEType lt ++ ":" ++ showELambda lt ,showEType rt ++ ":" ++ showELambda rt ]) 1

    let lt' = betaR (lv + 1) db lt
    let rt' = betaR (lv + 1) db rt

    EApply lt' rt'



betaR lv db (EFunction eb term) = do
    --let !_ = trace (mkdbg lv "Fun" [ "<" ,show eb ,showEType term ++ ":" ++ showELambda term ]) 1

    let term' = betaR (lv + 1) db term
    EFunction eb term'


betaR lv db org@(EBound eb) = do
    --let !_ = trace (mkdbg lv "Bou" [ ">" ,show eb ]) 1

    case lookup eb db of
        Just x -> betaR (lv + 1) db x
        _ -> do
            --let !_ = trace (mconcat ["Not Founc:", show eb]) 1
            org


betaR lv db org = do
    --let !_ = trace (mkdbg lv "Non" [ ">" ,show org ]) 1

    let !_ = error "Invalid Term Type !!"

    org


-- #--------------------------------------------------------------------------
-- #
-- #        a l p h a   c o n v e r s i o n
-- #
-- #--------------------------------------------------------------------------

genId :: State Int Int
genId = modify (+1) >> get


alpha :: Indent -> [(String, PTerm)] -> [(String, Bound)] -> PTerm -> State Int ETerm

alpha lv pdb edb (PApply lt rt) = do
    lt' <- alpha (lv + 1) pdb edb lt
    rt' <- alpha (lv + 1) pdb edb rt

    return $ EApply lt' rt'


alpha lv pdb edb (PFunction c term) = do
    -- α-変換に必要となる一意の ID を生成
    newId <- genId

    -- 束縛名は "元の変数名" + "#999"
    let eb = Bound (c:'#':show newId)

    let edb' = ([c], eb):edb
    term' <- alpha (lv + 1) pdb edb' term

    return $ EFunction eb term'


alpha lv pdb edb (PVar key) = do
    -- 束縛名と広域定義から参照先を見つける
    case lookup key edb of
        Just x -> return $ EBound x
        _ ->
            case lookup key pdb of
                Just x -> alpha (lv + 1) pdb edb x
                _ -> return $ ELabel key


-- #--------------------------------------------------------------------------
-- #
-- #        m a i n
-- #
-- #--------------------------------------------------------------------------


main = do
    {-
    print $ Function "f" (Function "g" (Function "x" (Apply (Var "f") (Apply (Var "g") (Var "x")))))
    print $ Function "f" (Function "g" (Function "x" (Apply (Apply (Var "f") (Var "g")) (Var "x"))))
    -}
    --P.parse parser "(src)" input

    input <- readFile "example.lmd"

    case P.runParser parser 1 "(src)" input of
        Right stmts -> executeStmts stmts
        Left err -> putStrLn "[parse error]" >> print err


    putStrLn "done."


-- EOF
