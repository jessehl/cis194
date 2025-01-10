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

-- Create a type class called Expr with three methods called lit, add,
-- and mul which parallel the constructors of ExprT. Make an instance of
-- Expr for the ExprT type, in such a way that
-- mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
-- == Mul (Add (Lit 2) (Lit 3)) (Lit 4)

class Expr a where
    lit :: Integer -> a
    add :: a -> a -> a
    mul :: a -> a -> a


instance Expr ExprT where
    lit :: Integer -> ExprT
    add = Add
    mul = Mul
    lit = Lit

foo :: ExprT 
foo = mul (add (lit 2) (lit 3)) (lit 4)