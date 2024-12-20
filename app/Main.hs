module Main where

import Week2.Week2
import Week2.Log (testParse)
import Data.Foldable (traverse_)

main :: IO ()
main = do
   messages <- testParse parse 1000000 "resources/Week2/error.log"
   _ <- traverse_ print (inorder (build messages))
   print (run myResult "1232e234234234")

