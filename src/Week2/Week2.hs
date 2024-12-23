{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE LambdaCase #-}

module Week2.Week2 where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (toList, Foldable (foldl'))
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Week2.Log (LogMessage (LogMessage) , MessageType (Error, Info, Warning))

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Foldable)

type Remainder = String

data ParseResult a = ParseError | Parsed a Remainder
  deriving (Show, Eq, Foldable)

newtype Parser a = Parser {run :: String -> ParseResult a}

zero :: Parser a
zero = Parser { run = const ParseError }

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = Parser $ \s -> case run pa s of
    ParseError -> ParseError
    Parsed a c -> Parsed (f a) c

instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser {run = Parsed a}

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) pf a = Parser $ \s -> case run pf s of
    ParseError    -> ParseError
    Parsed f rest -> run (fmap f a) rest

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) pa f = Parser $ \s -> case run pa s of
    ParseError    -> ParseError
    Parsed a rest -> run (f a) rest

item :: Parser Char
item = Parser $ \case
  []    -> ParseError 
  x: xs -> Parsed x xs

space :: Parser Char
space = satisfy (== ' ')

satisfy :: (Char -> Bool) -> Parser Char
satisfy cond = item >>= \x -> if cond x then pure x else zero

parseInt :: Parser Int
parseInt = fmap (foldl' (\acc x -> acc * 10 + x) 0 . fmap digitToInt) (multiple (parseList (satisfy isDigit)))

multiple :: Parser [a] -> Parser (NonEmpty a)
multiple pa = pa >>= \case
   x : xs -> pure (x :| xs)
   _      -> zero 

parseList :: Parser a -> Parser [a]
parseList parser = Parser $ \s -> case run parser s of
  ParseError  -> Parsed [] s
  Parsed x xs -> run (parseList parser & fmap (x :)) xs

getType :: Parser MessageType
getType = Parser $ \case
  'I' : xs       -> Parsed Info xs
  'W' : xs       -> Parsed Warning xs
  'E' : ' ' : xs -> run (parseInt & fmap Error) xs
  _              -> ParseError

parseMessage :: Parser LogMessage
parseMessage = do
  messageType <- getType
  _           <- space
  timeStamp   <- parseInt
  _           <- space
  LogMessage messageType timeStamp <$> remainder

remainder :: Parser String
remainder = Parser $ \s -> Parsed s []

parse :: String -> [LogMessage]
parse str = concatMap (toList . run parseMessage) (lines str)

insert :: LogMessage -> Tree LogMessage -> Tree LogMessage
insert message Leaf = Node Leaf message Leaf
insert newMessage@(LogMessage _ newTimestamp _) (Node l oldMessage@(LogMessage _ oldTimestamp _ ) r) =
    if newTimestamp < oldTimestamp
      then Node (insert newMessage l) oldMessage r
      else Node l oldMessage (insert newMessage r)

build :: [LogMessage] -> Tree LogMessage
build = foldl (flip insert) Leaf

inorder :: Tree LogMessage -> [LogMessage]
inorder = toList