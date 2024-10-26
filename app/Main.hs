module Main where

import  Week1 (toDigits)

main :: IO ()
main = do
  print "Hello, Haskell!"
  print (toDigits 2)