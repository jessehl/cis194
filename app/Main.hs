module Main where

import Week1
import Week2.Week2

main :: IO ()
main = do
  print "Hello, Haskell!"
  print (doubleEveryOtherB [23,32])
  print messageType