{-# LANGUAGE LambdaCase #-}
module Week5.Week5 where

import Week5.ExprT
import Week5.Parser(parseExp)

eval :: ExprT -> Integer
eval = \case
    Lit nr -> nr
    Mul a b -> eval a * eval b
    Add a b -> eval a + eval b

evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp Lit Add Mul str
