module Week1 where

toDigits :: Integer -> [Integer]
toDigits x 
  | x < 1 = []
  | x < 10 = [x]
  | otherwise = toDigits(x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev xs = reverse(toDigits xs)

doubleEveryOtherA :: [Integer] -> [Integer]
doubleEveryOtherA [] = []
doubleEveryOtherA [x] = [x * 2]
doubleEveryOtherA [x1,x2] = [x1 * 2, x2]
doubleEveryOtherA (x1:x2:xs) = [x1 * 2, x2] ++ doubleEveryOtherA xs

doubleEveryOtherB :: [Integer] -> [Integer]
doubleEveryOtherB lst = zipWith (*) lst (cycle [2, 1])

sumDigits :: [Integer]  -> Integer
sumDigits x = sum (x >>= toDigits)