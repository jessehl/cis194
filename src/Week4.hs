module Week4 where
import Data.List (foldl')

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1_ :: [Integer] -> Integer
fun1_ xs = product $ subtract 2 <$> filter even xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

logic2 :: Integer  -> Integer
logic2 x = if even x then x `div` 2 else 3 * x + 1

fun2_ :: Integer -> Integer
fun2_ n = sum $ filter even $ takeWhile (>1) $ iterate logic2 n

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Creates a balanced (but unsorted) Tree.
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Returns the Tree including a.
insert :: a -> Tree a -> Tree a
insert a Leaf = Node 0 Leaf a Leaf
insert a (Node int left value right) = if depth left >  depth right
  then Node int left value (insert a right)
  else Node (max int (depth newLeft + 1)) newLeft value right
  where
    newLeft = insert a left
    depth Leaf = 0
    depth (Node nr _ _ _) = nr

maybeHead :: Tree a -> Maybe (a, Integer)
maybeHead t = case t of
   Leaf -> Nothing
   Node int _ a _ -> Just (a, int)

xor :: [Bool] -> Bool
xor = foldl' (\acc x -> if not x then acc else not acc) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a acc -> (f a) : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f)  base (reverse xs)


-- condition: i + j + 2*i*j ≤ n
-- so the max usable j value (i.e. when i = 1) makes the condition:
-- 1 + j + 2*1*j ≤ n
-- 1 + 3*j       ≤ n
-- 1/3 + j       ≤ n/3
-- j             ≤ n/3 - (1/3)
toRemove :: Int -> [Int]
toRemove n = concatMap (takeWhile (<=n)) $ do
  j <- [1..(n `div` 3)] 
  pure $ (\i -> i + j + (2 * j * i)) <$> [1..j]

toRemove1 :: Int -> [Int]
toRemove1 n = takeWhile (<=n) [i + j + (2 * j * i) | j <- [1..(n `div` 3)], i <- [1..j]]

sieveSundaram :: Int -> [Int]
sieveSundaram n = [(x * 2) + 1 | x <- [1..n], x `notElem` toRemove1 n]