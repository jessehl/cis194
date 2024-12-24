module Week3.Golf where
import Data.List ( sortBy, sort )
import Data.Ord (comparing, Down (Down))
import Data.List.NonEmpty (group, head)
import GHC.Base (NonEmpty)
import Data.Foldable (find)
import Data.List (transpose)

skips :: [a] -> [[a]]
skips xs = [[val | (idx, val) <- zip [1..] xs, idx `mod` n == 0] | n <- fmap snd (zip xs [1..])]

localMaxima :: [Integer] -> [Integer]
localMaxima lst = sortBy (comparing Data.Ord.Down) $ concatMap (\(left, this, right) -> ([this | this > left && this > right])) (zip3 lst (tail lst) (tail $ tail lst))


histogram :: [Integer] -> String
histogram xa = unlines $ fmap concat $ matrix xa

grouped :: (Eq a, Ord a) => [a] -> [NonEmpty a]
grouped xs = group $ sort xs

counts :: (Eq a, Ord a) => [a] -> [(a, Int)]
counts xa = fmap (\x -> (Data.List.NonEmpty.head x, length x)) $ grouped xa

matrix :: [Integer] -> [[String]]
matrix xa = reverse $ transpose $  do
  nr <- [0..9]
  let maybeScore = find (\(number, _) -> nr == number) $ counts xa
  let score      = maybe 0 snd maybeScore
  let scoreBar   = show nr : "=" : replicate score "*" ++ replicate (10  - score) " "
  pure scoreBar