module MyModule where

greet :: String -> IO ()
greet cs = putStrLn $ "[" ++ cs ++ "]"
