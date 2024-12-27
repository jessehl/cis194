module Week3.Golf where
import Data.List (sortBy, sort, transpose)
import Data.Ord (comparing, Down (Down))
import Data.List.NonEmpty (group, head)
import Data.Foldable (find)

skips :: [a] -> [[a]]
skips xs = [[val | (idx, val) <- zip [1..] xs, idx `mod` n == 0] | n <- fmap snd (zip xs [1..])]

localMaxima :: [Integer] -> [Integer]
localMaxima lst = sortBy (comparing Data.Ord.Down) $ concatMap (\(left, this, right) -> ([this | this > left && this > right])) (zip3 lst (tail lst) (tail $ tail lst))


histogram :: [Int] -> String
histogram xa = unlines $ concat <$> matrix xa

matrix :: [Int] -> [[String]]
matrix xa = reverse $ transpose $  do
  let scores     = fmap (\x -> (Data.List.NonEmpty.head x, length x)) $ group $ sort xa
  let maxScore   = foldl max 0 $ fmap snd scores
  nr             <- [0..9]
  let score      = maybe 0 snd (find (\(number, _) -> nr == number) scores)
  let scoreBar   = take (maxScore + 2) $ show nr : "=" : replicate score "*" ++ repeat " "
  pure scoreBar