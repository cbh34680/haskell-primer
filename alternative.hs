import Control.Applicative

f = do
    s <- readFile "/notfound" <|> readFile "/etc/passwd"
    putStrLn s
