module Week1 where

toDigits :: Integer -> [Integer]
toDigits x 
  | x < 1 = []
  | x < 10 = [x]
  | otherwise = toDigits(x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev xs = reverse(toDigits xs)

doubleEveryOtherB :: [Integer] -> [Integer]
doubleEveryOtherB lst = zipWith (*) lst (if even(length lst) then cycle [2, 1] else cycle[1,2])

sumDigits :: [Integer]  -> Integer
sumDigits x = sum (x >>= toDigits)

validate :: Integer -> Bool
validate x =  sumDigits (doubleEveryOtherB (toDigits x)) `mod` 10 == 0