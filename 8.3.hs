data Tree a = Leaf a | Node (Tree a) (Tree a)

nleaves :: Tree a -> Int
nleaves (Leaf _) = 1
nleaves (Node l r) = nleaves l + nleaves r

t :: Tree Int
t = Node (Node (Node (Node (Leaf 1) (Leaf 2)) (Leaf 2)) (Leaf 4))
         (Node (Leaf 6) (Leaf 9))

balanced :: Tree a -> Bool
balanced (Leaf _)   = True
balanced (Node l r) = abs (nleaves l - nleaves r) <= 1
