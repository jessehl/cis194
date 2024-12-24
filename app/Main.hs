module Main where

import Week2.Week2
import Week2.Log (testParse)
import Week3.Golf (histogram)
import Data.Foldable (traverse_)

main :: IO ()
main = do
   messages <- testParse parse 1000000 "resources/Week2/error.log"
   traverse_ print (inorder (build messages))
   putStr (histogram [1, 4,4,4,4, 5, 3])

