import Control.Monad
import Control.Applicative
import Data.Char (isAsciiLower, isSpace)
import Data.Maybe (catMaybes)
import Data.List ((\\), group, sort)
import Data.List.Extra (notNull)
import qualified Data.Map as Map
import qualified Text.Parsec as P


data Term = Var String | Function Char Term | Apply Term Term deriving (Eq, Show)
data Stmt = Define (String, Term) | Execute Term deriving (Eq, Show)


showLambda (Var cs) = cs
showLambda (Function c term) = mconcat ["(\\", [c], ".", showLambda term, ")"]
showLambda (Apply t1 t2) = mconcat ["(", showLambda t1, " ", showLambda t2, ")"]


separator = P.oneOf " \t"
skipSpaces = void (P.skipMany separator)


parseIdent = P.many1 P.letter <* skipSpaces
parseArgs = P.many1 (P.satisfy isAsciiLower <|> P.char ' ') <* skipSpaces
-- parseLetter = P.letter <* skipSpaces

eol = P.char '\n'

literal c = P.char c <* skipSpaces


--parser :: P.Parsec String () Term
parser = skipSpaces *> (catMaybes <$> parseStmt `P.sepBy` separator) <* P.eof


--parseTerm :: P.Parsec String () Term
parseStmt = (P.try parseDefine <|> parseExecute <|> emptyLine) <* eol

{-
parseDefine = do
    ident <- parseIdent
    literal '='
    apply <- parseApply

    return $ Just $ Define (ident, apply)
-}
--parseDefine = Just . Define <$> ((,) <$> (parseIdent <* literal '=') <*> parseApply)
parseDefine = ((Just . Define) .) . (,) <$> (parseIdent <* literal '=') <*> parseApply


parseExecute = Just . Execute <$> parseApply


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

    return $ foldl (\acc c -> Function c acc) (Function x apply) xs


parseVar = Var <$> parseIdent


executeStmts stmts = do
    let execs = filter isExecute stmts
    let defs = map (\(Define t) -> t) $ stmts \\ execs

    let dups = map (!! 0) . filter ((> 1) . length) . group . sort $ map fst defs
    when (notNull dups) (error (mconcat ["duplicate terms (", show dups, ")"]))
    --guard (notnull dups)

    --print $ Map.fromList defs
    --print execs

    traverse (putStrLn . ((\k v -> k ++ "=" ++ v) <$> fst <*> showLambda . snd)) defs

    putStrLn "execute!"


isExecute :: Stmt -> Bool
isExecute (Execute _) = True
isExecute _ = False


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
\ aa = \\f.\\g.\\x. f (g x)  \n\
\ ab = \\fgx. f (g x)  \n\
\ ac = \\f g x. f (g x)  \n\
\ ad = \\f.(\\g.(\\x. f (g x)))  \n\
\  \n\
\ bb = (aa f g)     \n\
\ cc = (aa f g)     \n\
\  \n\
\ (aa f g)     \n\
\"



-- EOF
