module Main where

import  Week1

main :: IO ()
main = do
  print "Hello, Haskell!"
  print (doubleEveryOtherB [23,32])