module Main where

import Week2.Week2
import Week2.Log (testParse)
import Week4(foldTree, maybeHead)
import Data.Foldable (traverse_)

main :: IO ()
main = do
   messages <- testParse parse 1 "resources/Week2/error.log"
   traverse_ print (inorder (build messages))
   let tree = foldTree "ABCDEFGHIJ"
   print $ maybeHead tree 
   print tree
   

