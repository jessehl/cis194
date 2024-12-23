module Week3.Golf where

skips :: [a] -> [[a]]
skips xs = do 
  (n, _) <- indexed
  pure [val | (idx, val) <- indexed, idx `mod` n == 0]
  where indexed = zip [1..] xs