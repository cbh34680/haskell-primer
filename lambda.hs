import Control.Applicative
import Control.Monad
import Control.Monad.Except

import Data.Foldable (traverse_)
import Data.Char (isAsciiLower, isSpace)
import Data.Maybe (catMaybes)
import Data.List ((\\), group, sort)
import Data.List.Extra (notNull)

import qualified Data.Map as Map
import qualified Text.Parsec as P


data Term = Var String | Function {boundVar::Char, body::Term} | Apply {func::Term, arg::Term} deriving (Eq, Show)
data Stmt = Define (String, Term) | Eval Term deriving (Eq, Show)


showLambda (Var cs) = cs
showLambda (Function c term) = mconcat ["(\\", [c], ".", showLambda term, ")"]
showLambda (Apply t1 t2) = mconcat ["(", showLambda t1, " ", showLambda t2, ")"]


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
parseStmt = skipSpaces *> (P.try parseDefine <|> parseEval <|> emptyLine) <* eol
--parseStmt = (P.try parseDefine <|> parseEval <|> emptyLine) <* eol

{-
parseDefine = do
    ident <- parseIdent
    literal '='
    apply <- parseApply

    return $ Just $ Define (ident, apply)
-}
--parseDefine = Just . Define <$> ((,) <$> (parseIdent <* literal '=') <*> parseApply)
parseDefine = ((Just . Define) .) . (,) <$> (parseIdent <* literal '=') <*> parseApply


parseEval = Just . Eval <$> parseApply


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
    arg <- parseLetter
    literal '.'

    Function [arg] <$> parseApply
-}
parseFunction = do
    literal '\\'
    args <- filter (not . isSpace) <$> parseArgs
    literal '.'
    apply <- parseApply

    let (x:xs) = reverse args

    --return $ foldl (\acc c -> Function c acc) (Function x apply) xs
    return $ foldl (flip Function) (Function x apply) xs


parseVar = Var <$> parseIdent


executeStmts stmts = do
    let execs = filter isEval stmts
    let defs = map (\(Define t) -> t) $ stmts \\ execs

    let dups = map (!! 0) . filter ((> 1) . length) . group . sort $ map fst defs

    when (notNull dups) (error (mconcat ["duplicate terms (", show dups, ")"]))
    --guard (notnull dups)

    --print $ Map.fromList defs
    --print execs

    putStrLn "---------- haskell ----------"
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> show . snd)) defs
    putStrLn ""
    traverse_ print execs
    putStrLn ""
    putStrLn "---------- defines ----------"
    traverse_ (putStrLn . ((\k v -> k ++ ": " ++ v) <$> fst <*> showLambda . snd)) defs
    putStrLn ""
    putStrLn "---------- execute ----------"
    print execs
    putStrLn ""
    putStrLn "execute!"

    where
        isEval :: Stmt -> Bool
        isEval (Eval _) = True
        isEval _ = False


main = do
    {-
    print $ Function "f" (Function "g" (Function "x" (Apply (Var "f") (Apply (Var "g") (Var "x")))))
    print $ Function "f" (Function "g" (Function "x" (Apply (Apply (Var "f") (Var "g")) (Var "x"))))
    -}
    --P.parse parser "(src)" input

    case P.parse parser "(src)" input of
        Right stmts -> executeStmts stmts
        Left err -> putStrLn "[parse error]" >> print err


    putStrLn "done."


input = "\
\  \n\
\  succ  = \\n. \\f. \\x. f (n f x)   \n\
\  c0    = \\f. \\x. x                \n\
\  c1    = succ zero                  \n\
\  c2    = succ (succ zero)           \n\
\  \n\
\  c1    \n\
\"



-- EOF
