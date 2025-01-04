module Week4 where

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
insert a (Node int left value right) = case (left, right) of
  (Leaf, _)                    -> Node 1 newNode value right
  (_, Leaf)                    -> Node 1 left value newNode
  _ | depth left > depth right -> Node int left value (insert a right)
  _                            -> Node (max int (depth newLeft + 1)) newLeft value right
  where
    newNode    = Node 0 Leaf a Leaf
    newLeft    = insert a left
    depth Leaf = 0
    depth (Node nr _ _ _) = nr

maybeHead :: Tree a -> Maybe (a, Integer)
maybeHead t = case t of 
   Leaf -> Nothing 
   Node int _ a _ -> Just (a, int)