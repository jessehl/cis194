module Week1 where

import Data.List.NonEmpty qualified as NonEmpty

toDigits :: Integer -> [Integer]
toDigits x
  | x < 1 = []
  | x < 10 = [x]
  | otherwise = toDigits (x `div` 10) ++ [x `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev xs = reverse (toDigits xs)

doubleEveryOtherB :: [Integer] -> [Integer]
doubleEveryOtherB lst = zipWith (*) lst (if even (length lst) then cycle [2, 1] else cycle [1, 2])

sumDigits :: [Integer] -> Integer
sumDigits x = sum (x >>= toDigits)

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOtherB (toDigits x)) `mod` 10 == 0

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a _ c = [(a, c)]
hanoi n a b c = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n - 1) b a c
