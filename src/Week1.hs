module Week1 (toDigits, toDigitsRev) where

toDigits :: Integer -> [Integer]
toDigits x 
  | x < 1 = []
  | x < 10 = [x]
  | otherwise = toDigits(x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev xs = reverse(toDigits xs)

