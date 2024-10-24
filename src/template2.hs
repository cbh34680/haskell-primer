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

    let xx = $(qExt4 "f")
    print xx

    print $(qExp5)

    let yy = $(qExp6 "fa3" [1,2,3])
    print yy

    zz <- runQ (qExp6 "fa3" [1,2,3])
    print zz

    putStrLn "done."






-- EOF
