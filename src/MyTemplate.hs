{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}

module MyTemplate where

import Language.Haskell.TH


myFunc :: Q Exp
myFunc = do
  x <- newName "x"
  return $ LamE
    [VarP x]
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (LitE (IntegerL 1))))

myFunc2 :: Q Exp
myFunc2 = do
  x <- newName "x"
  y <- newName "y"

  return $ LamE
    [VarP x, VarP y]
    (InfixE (Just (VarE x)) (VarE '(+)) (Just (VarE y)))

    -- \x y -> x + y


-- EOF
