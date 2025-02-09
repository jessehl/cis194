module Main where

import Week6

main :: IO ()
main = do
   let ones :: Stream Int = streamRepeat 1
   print $ streamMap (+100) ones