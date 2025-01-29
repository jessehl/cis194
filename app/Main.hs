module Main where

import Week5.Week5

main :: IO ()
main = do
   print $ compileAndRun "(5 + (0 * (1 + 0) * 10 + -100 + -0 + 000 + 1 + -0 * 1)) * 10000"