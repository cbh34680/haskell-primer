{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import MyTemplate2

doExp1 = $(qExp1)

main = do

    exp <- runQ [| 109 + 209 |]
    print exp

    putStrLn ">>>"
    $([| print $(varE (mkName "exp")) |])
    putStrLn "<<<"

    print doExp1

    print $(qExp2)

    print $(qExp3)

    let f = $(qExt4 "f")

    putStrLn "done."


