module Main where

import Week6
import Data.Foldable (toList)

main :: IO ()
main = do
   let ones:: Stream Int = Cons 1 ones
   print $ take 1000 $ toList ones