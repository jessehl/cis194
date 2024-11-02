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

-- Implementation where we have multiple spare pegs.
hanoi' :: Int -> Int -> NonEmpty.NonEmpty Int -> Int -> [(Int, Int)]
hanoi' 0 _ _ _ = []
hanoi' 1 sourcePeg _ targetPeg = [(sourcePeg, targetPeg)]
hanoi' nDisks sourcePeg sparePegs targetPeg
  -- If enough spares, we move all but the last of the source disks onto the spare pegs.
  -- Then, move the remaining source disk onto the target, followed by the pegs just placed on the spares.
  | nDisks <= length sparePegs = map (sourcePeg,) sparesUsed ++ [(sourcePeg, targetPeg)] ++ map (,targetPeg) (reverse sparesUsed)
  | otherwise =
      -- Move n-1 disks to the first spare peg, using the target peg as extra spare.
      hanoi' (nDisks - 1) sourcePeg (targetPeg NonEmpty.:| NonEmpty.tail sparePegs) (NonEmpty.head sparePegs)
        ++
        -- Move the top disk to the target peg.
        [(sourcePeg, targetPeg)]
        ++
        -- Move n-1 disks from the first spare peg to the target, using the source as extra spare.
        hanoi' (nDisks - 1) (NonEmpty.head sparePegs) (sourcePeg NonEmpty.:| NonEmpty.tail sparePegs) targetPeg
  where
    sparesUsed = NonEmpty.take (nDisks - 1) sparePegs