module Main where

import Week2.Week2
import Week2.Log (testParse)
import Data.Function ((&))
import qualified Control.Monad

main :: IO ()
main = do
  print $ parseMessage "I 200 sfdsdf hahaha"
  messages <- testParse parse 10 "resources/Week2/error.log"
  traverse print messages & Control.Monad.void
