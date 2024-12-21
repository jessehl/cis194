{-# LANGUAGE DeriveFoldable #-}

module Week2.Week2 where

import Data.Bifunctor qualified
import Data.Char (digitToInt, isDigit)
import Data.Foldable (toList)
import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Week2.Log (LogMessage (LogMessage, Unknown), MessageType (Error, Info, Warning))

parse :: String -> [LogMessage]
parse str = str & lines & fmap parseMessage

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Foldable)

data ParseResult a = ParseError | Parsed a String
  deriving (Show)

newtype Parser a = Parser {run :: String -> ParseResult a}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = Parser $ \s -> case run pa s of
    ParseError -> ParseError
    Parsed a c -> Parsed (f a) c

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser {run = Parsed a}

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pab a = Parser $ \s -> case run pab s of
    ParseError -> ParseError
    Parsed f rest -> run (fmap f a) rest

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = Parser $ \s -> case run pa s of
    ParseError -> ParseError
    Parsed a rest -> run (f a) rest

myResult :: Parser (Int, Int, Int)
myResult = do
  first <- parseInt
  second <- parseInt
  third <- parseInt
  pure (first, second, third)

parseInt :: Parser Int
parseInt = Parser $ \s -> case s of
  s : xs -> Parsed (digitToInt s) xs
  _ -> ParseError

parseMessage :: String -> LogMessage
parseMessage line = fromMaybe (Unknown line) $ do
  (messageType, remainder1) <- getType line
  (timeStamp, remainder2) <- stripLeftInt remainder1
  Just (LogMessage messageType timeStamp remainder2)

consumeInt :: Int -> [Char] -> Maybe (Int, [Char])
consumeInt acc [] = Just (acc, [])
consumeInt acc (x : xs)
  | x == ' ' = Just (acc, xs)
  | isDigit x = consumeInt (digitToInt x + acc * 10) xs
  | otherwise = Nothing

stripLeftInt :: [Char] -> Maybe (Int, [Char])
stripLeftInt [] = Nothing
stripLeftInt str = consumeInt 0 str

getType :: String -> Maybe (MessageType, String)
getType line = case line of
  'I' : ' ' : xs -> Just (Info, xs)
  'W' : ' ' : xs -> Just (Warning, xs)
  'E' : ' ' : xs -> stripLeftInt xs & fmap (Data.Bifunctor.first Error)
  _ -> Nothing

insert :: LogMessage -> Tree LogMessage -> Tree LogMessage
insert (Unknown _) tree = tree
insert newMessage@(LogMessage _ newTimestamp _) tree = case tree of
  Leaf -> Node Leaf newMessage Leaf
  Node l (Unknown _) r -> Node l newMessage r
  Node l oldMessage@(LogMessage _ oldTimestamp _) r ->
    if newTimestamp < oldTimestamp
      then Node (insert newMessage l) oldMessage r
      else Node l oldMessage (insert newMessage r)

build :: [LogMessage] -> Tree LogMessage
build = foldl (flip insert) Leaf

inorder :: Tree LogMessage -> [LogMessage]
inorder = toList