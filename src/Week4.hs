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
foldTree = foldr (\z a -> fst $ insert z a) Leaf

-- Inserts an 'a' into the Tree, returning the new Tree, and the number of levels added by the insert.
insert :: a -> Tree a -> (Tree a, Integer)
insert a ta = case ta of
  Leaf -> (Node 0 Leaf a Leaf, 1)
  Node int left value right -> case (left, right) of
    (Leaf, Leaf)                           -> (Node 1 newNode value right, 1)
    (Leaf, Node{})                         -> (Node int newNode value right, 0)
    (Node{}, Leaf)                         -> (Node int left value newNode, 0)
    (Node l _ _ _, Node r _ _ _) |  l < r  -> (Node int newL value right, 0)
    (Node l _ _ _, Node r _ _ _) |  l > r  -> (Node int left value newR, 0)
    (Node {}, Node {})                     -> (Node (int + intL) newL value right, intL)
    where
      newNode     =  Node 0 Leaf a Leaf
      (newL, intL) = insert a left
      (newR, _)    = insert a right

maybeHead :: Tree a -> Maybe (a, Integer)
maybeHead t = case t of 
   Leaf -> Nothing 
   Node int _ a _ -> Just (a, int)