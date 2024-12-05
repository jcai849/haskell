data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

repeat :: a -> Tree a
repeat x = Node (Main.repeat x) x (Main.repeat x)

-- Not as simple as taking n `div` 2 of left and right -- tree not
-- necessarily balanced; Must keep track of nodes taken
take :: Int -> Tree a -> Tree a
take n t = t' where (_, t') = take' n t

take' :: Int -> Tree a -> (Int, Tree a)
take' 0 t = (0, Leaf)
take' n Leaf = (n, Leaf)
take' n (Node l x r) = (rn, Node l' x r')
  where
    (ln, l') = take' (n - 1) l
    (rn, r') = take' ln r

replicate :: Int -> a -> Tree a
replicate n = Main.take n . Main.repeat

x = Node (Node (Node Leaf 1 Leaf) 2 Leaf) 3 (Node Leaf 4 Leaf)
