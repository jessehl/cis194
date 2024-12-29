module Week4 where

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1_ :: [Integer] -> Integer
fun1_ xs = product $ subtract 2 <$> filter even xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

logic2 :: Integer  -> Integer 
logic2 x = if even x then x `div` 2 else 3 * x + 1

fun2_ :: Integer -> Integer
fun2_ n = sum $ filter even $ takeWhile (>1) $ iterate logic2 n