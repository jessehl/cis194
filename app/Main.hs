module Main where

import Week6

main :: IO ()
main = do
   let ones:: Stream Int = Cons 1 ones
   print ones