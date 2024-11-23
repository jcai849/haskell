data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving (Show)

instance Functor Tree where
  fmap g Leaf = Leaf
  fmap g (Node l f r) = Node (fmap g l) (g f) (fmap g r)
