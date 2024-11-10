module Week2.Week2 where

import Data.Function ((&))
import Week2.Log
import qualified Data.Bifunctor
import Data.Char (isDigit, digitToInt)


parseMessage :: String -> LogMessage
parseMessage _ = undefined

consumeInt :: Int -> [Char] -> Maybe(Int, [Char])
consumeInt acc [] = Just(acc, []) 
consumeInt acc (x1:xs)
    | x1 == ' '   = Just(acc, [])
    | isDigit x1  = consumeInt (digitToInt x1 + acc * 10) xs
    | otherwise   = Nothing
         

getType :: String -> Maybe (MessageType, String)
getType line = case line of
  'I' : ' ' : xs -> Just (Info, xs)
  'W' : ' ' : xs -> Just (Warning, xs)
  'E' : ' ' : xs -> consumeInt 0 xs & fmap (Data.Bifunctor.first Error)
  _ -> Nothing
