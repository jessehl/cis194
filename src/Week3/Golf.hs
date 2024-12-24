module Week3.Golf where

skips :: [a] -> [[a]]
skips xs = [[val | (idx, val) <- zip [1..] xs, idx `mod` n == 0] | n <- fmap snd (zip xs [1..])]