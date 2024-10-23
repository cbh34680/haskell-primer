{-# LANGUAGE TemplateHaskell #-}

module MyTemplate2 where

import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Debug.Trace


-- [| |] は Q Exp を返す
qExp1 :: Q Exp
qExp1 = [| 109 + 209 |]

-- 小文字から始まる infixE は Q Exp を返す
qExp2 :: Q Exp
qExp2 = infixE (Just (litE (IntegerL 109))) (varE '(+)) (Just (litE (integerL 209)))

-- 大文字から始まる InfixE は Exp なので、return で Q モナドにする
qExp3 :: Q Exp
qExp3 = return $ InfixE (Just (LitE (IntegerL 109))) (VarE '(+)) (Just (LitE (IntegerL 209)))

f = 1

qExt4 :: String -> Q Exp
qExt4 cs = do
    xx <- lookupValueName "f"
    let !_ = trace (concat ["1!!!!", show xx, "!!!!!"]) 1

    let fn = mkName cs
    let !_ = trace (concat ["2!!!!", showName fn, "!!!!!"]) 1

    --return $ VarE (fromJust xx)
    return $ VarE fn

qExp5 :: Q Exp
qExp5 = [| $(litE (integerL 109)) + 209 |]


fa3 a b c = 3


qExp6 :: String -> [Int] -> Q Exp
qExp6 cs ns = do
    let fn = mkName cs

    return $ foldl (\acc x -> AppE acc (LitE (IntegerL (fromIntegral x)))) (VarE fn) ns








-- EOF

