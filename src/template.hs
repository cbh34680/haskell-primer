{-# LANGUAGE TemplateHaskell #-}

import qualified Language.Haskell.TH as TH

genHello :: TH.Q TH.Exp
genHello = TH.litE (TH.stringL "Hello, Template Haskell!")


main :: IO ()
main = do
    print =<< TH.runQ [e| 109 + 209 |]
    print =<< TH.runQ [d| x = 309 |]

    code <- TH.runQ genHello

    print code
    putStrLn $ TH.pprint code

