import Control.Monad
import Control.Applicative
import Data.Maybe (catMaybes)
import Data.List ((\\), group, sort)
import Data.List.Extra (notNull)
import qualified Data.Map as Map
import qualified Text.Parsec as P


data Term = Var String | Function String Term | Apply Term Term deriving (Eq)
data Stmt = Define (String, Term) | Execute Term deriving (Eq, Show)


instance Show Term where
    show (Var cs) = cs
    show (Function cs term) = mconcat ["(\\", cs, ".", show term, ")"]
    show (Apply t1 t2) = mconcat ["(", show t1, " ", show t2, ")"]


separator = P.oneOf " \t"
skipSpaces = void (P.skipMany separator)

parseIdent = P.many1 P.letter <* skipSpaces

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


emptyLine = do
    skipSpaces

    return Nothing


parseApply = do
    term <- parseTerm

    do
        terms <- P.many1 parseTerm
        --return $ foldl (\ls x -> Apply ls x) term terms
        return $ foldl Apply term terms

        <|> do
            return term


parseTerm = nested <|> function <|> var


nested = literal '(' *> parseApply <* literal ')'

function = Function <$> (literal '\\' *> parseIdent <* literal '.') <*> parseApply

var = Var <$> parseIdent

executeStmts stmts = do
    let execs = filter isExecute stmts
    let defs = map (\(Define t) -> t) $ stmts \\ execs

    let dups = map (!! 0) . filter ((> 1) . length) . group . sort $ map fst defs
    when (notNull dups) (error (mconcat ["duplicate terms (", show dups, ")"]))
    --guard (notnull dups)

    print $ Map.fromList defs
    print execs

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
        _ -> putStrLn "parse error"


    putStrLn "done."


input = "\
\ aa = \\f.\\g.\\x. f (g x)  \n\
\  \n\
\ bb = (aa f g)     \n\
\ cc = (aa f g)     \n\
\  \n\
\ (aa f g)     \n\
\"



-- EOF
