module Week2.Week2 where

import Data.Function ((&))
import Week2.Log
import qualified Data.Bifunctor
import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromMaybe)

parseMessage :: String -> LogMessage
parseMessage line = fromMaybe (Unknown line) $ do 
  (messageType, remainder1) <- getType line
  (timeStamp, remainder2) <- stripLeftInt remainder1
  Just (LogMessage messageType timeStamp remainder2)
 
consumeInt :: Int -> [Char] -> Maybe(Int, [Char])
consumeInt acc [] = Just(acc, []) 
consumeInt acc (x:xs)
    | x == ' '   = Just(acc, xs)
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
