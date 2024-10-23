{-# LANGUAGE TemplateHaskell #-}

module MyTemplate2 where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Debug.Trace


qExp1 :: Q Exp
qExp1 = [| 109 + 209 |]

qExp2 :: Q Exp
qExp2 = infixE (Just (litE (IntegerL 109))) (varE '(+)) (Just (litE (integerL 209)))

qExp3 :: Q Exp
qExp3 = return $ InfixE (Just (LitE (IntegerL 109))) (VarE '(+)) (Just (LitE (IntegerL 209)))

f = 1

qExt4 :: String -> Q Exp
qExt4 cs = do
    xx <- lookupValueName "f"
    let !_ = trace (concat ["!!!!!", show xx, "!!!!!"]) 1

    let fn = mkName cs

    return $ VarE fn



-- EOF

