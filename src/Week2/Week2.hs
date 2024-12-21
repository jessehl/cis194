{-# LANGUAGE DeriveFoldable #-}

module Week2.Week2 where

import Data.Function ((&))
import Week2.Log (LogMessage (LogMessage, Unknown), MessageType (Info, Warning, Error))
import qualified Data.Bifunctor
import Data.Char (isDigit, digitToInt)
import Data.Maybe (fromMaybe)
import Data.Foldable (toList)

parse :: String -> [LogMessage]
parse str = str & lines & fmap parseMessage

data Tree a = Leaf
  | Node (Tree a) a (Tree a)
  deriving Foldable


data ParseResult a = ParseError | Parsed a String
  deriving Show

newtype Parser a = Parser { run :: String -> ParseResult a }

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f = Parser $ \s ->
  case run p s of
    ParseError -> ParseError
    Parsed a rest -> run (f a) rest

create :: a -> Parser a
create a = Parser { run = Parsed a }


fmap1 :: (a -> b) -> Parser a -> Parser b 
fmap1 f pa = Parser $ \s -> case run pa s of 
      ParseError -> ParseError  
      Parsed a c -> Parsed (f a) c

instance Functor Parser where 
  fmap = fmap1

-- Applicative instance
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser { run = Parsed a }

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p a = Parser $ \s -> case run p s of 
      ParseError    -> ParseError 
      Parsed f rest -> run (fmap f a) rest


instance Monad Parser where
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) = bind


getInt :: Parser Int
getInt = create 1

myResult:: Parser (Int, Int, Int) 
myResult = do 
  first  <- parseInt 
  second <- parseInt 
  third  <- parseInt 
  create (first, second, third)

parseInt :: Parser Int 
parseInt = Parser $ \s -> case s of 
   s : xs -> Parsed (digitToInt s) xs
   _      -> ParseError






parseMessage :: String -> LogMessage
parseMessage line = fromMaybe (Unknown line) $ do
  (messageType, remainder1) <- getType line
  (timeStamp, remainder2) <- stripLeftInt remainder1
  Just (LogMessage messageType timeStamp remainder2)

consumeInt :: Int -> [Char] -> Maybe(Int, [Char])
consumeInt acc [] = Just (acc, [])
consumeInt acc (x:xs)
    | x == ' '   = Just (acc, xs)
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


insert :: LogMessage -> Tree LogMessage -> Tree LogMessage
insert (Unknown _) tree = tree
insert newMessage@(LogMessage _ newTimestamp _) tree = case tree of
   Leaf                  -> Node Leaf newMessage Leaf
   Node l (Unknown _) r  -> Node l newMessage r
   Node l oldMessage@(LogMessage _ oldTimestamp _) r ->
     if newTimestamp < oldTimestamp then Node (insert newMessage l) oldMessage r
     else Node l oldMessage (insert newMessage r)


build :: [LogMessage] -> Tree LogMessage
build = foldl (flip insert) Leaf

inorder :: Tree LogMessage -> [LogMessage]
inorder = toList