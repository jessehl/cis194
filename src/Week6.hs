module Week6 where
import Data.Foldable (Foldable(toList))
import Data.List (intercalate)
import GHC.Base (Opaque(O))

fib :: Integer -> Integer

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : go 0 1
  where
    go n1 n2 = n3 : go n2 n3
       where n3 = n1 + n2


data Stream a = Cons a (Stream a) deriving Foldable

instance Show a => Show (Stream a) where
    show stream =  "Stream(" ++ intercalate ", " (take 20 (fmap show (toList stream))) ++ ", ..., ∞)"

streamRepeat :: a -> Stream a 
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b 
streamMap f (Cons a remainder) = Cons (f a) (streamMap f remainder)