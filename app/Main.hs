module Main where

import Week2.Week2
import Week2.Log (testParse)
import Data.Foldable (traverse_)

main :: IO ()
main = do
   messages <- testParse parse 10 "resources/Week2/error.log"
   traverse_ print messages
