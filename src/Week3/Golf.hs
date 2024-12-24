module Week3.Golf where
import Data.List (sortBy)
import Data.Ord (comparing, Down (Down))

skips :: [a] -> [[a]]
skips xs = [[val | (idx, val) <- zip [1..] xs, idx `mod` n == 0] | n <- fmap snd (zip xs [1..])]

localMaxima :: [Integer] -> [Integer]
localMaxima lst = sortBy (comparing Data.Ord.Down) $ concatMap (\(left, this, right) -> ([this | this > left && this > right])) (zip3 (tail $ tail lst) (tail lst) lst)
