module Main where

import Week2.Week2
import Week2.Log (testParse)

main :: IO ()
main = do
   messages <- testParse parse 100 "resources/Week2/error.log"
   print (sortMessages messages)
