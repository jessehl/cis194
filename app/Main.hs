module Main where

import Week7.Editor(runEditor, getCurLine)




main :: IO ()
main = do 
   res <- runEditor getCurLine "329999999\n234"
   print res