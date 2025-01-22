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
    add = Add
    mul = Mul
    lit = Lit

instance Expr Integer where
    add = (+)
    mul = (*)
    lit a = a


instance Expr Bool where 
    add = (||)
    mul = (&&)
    lit = (<0)

instance Expr MinMax where
    add = max
    mul = min
    lit = MinMax

instance Expr Mod7 where
    add a1 a2 = mod (a1 + a2) 7
    mul a1 a2 = (a1 * a2) `mod` 7
    lit = Mod7



-- Why the need for the deriving clauses here? Doesn't Integer already have instances for those (type) classes?
newtype Mod7 = Mod7 Integer deriving (Eq, Show, Enum, Integral, Real, Ord, Num) 
newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)

expression :: Mod7
expression = mul (add (lit 2) (lit 3)) (lit 4)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7



