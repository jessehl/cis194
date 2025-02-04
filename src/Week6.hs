module Week6 where

fib :: Integer -> Integer

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : go 0 1
  where 
    go l1 l2 = nw : go l2 nw 
       where nw = l1 + l2