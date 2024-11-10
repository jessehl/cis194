module Week2.Week2 where

import Data.Function ((&))
import Week2.Log
import qualified Data.Bifunctor
import Data.Char (isDigit, digitToInt)

parseMessage :: String -> LogMessage
parseMessage _ = undefined

consumeInt :: Int -> [Char] -> Maybe(Int, [Char])
consumeInt acc [] = Just(acc, []) 
consumeInt acc (x:xs)
    | x == ' '   = Just(acc, [])
    | isDigit x  = consumeInt (digitToInt x + acc * 10) xs
    | otherwise  = Nothing

stripLeftInt :: [Char] -> Maybe(Int, [Char])
stripLeftInt []  = Nothing
stripLeftInt str = consumeInt 0 str
         

getType :: String -> Maybe (MessageType, String)
getType line = case line of
  'I' : ' ' : xs -> Just (Info, xs)
  'W' : ' ' : xs -> Just (Warning, xs)
  'E' : ' ' : xs -> stripLeftInt xs & fmap (Data.Bifunctor.first Error)
  _ -> Nothing
