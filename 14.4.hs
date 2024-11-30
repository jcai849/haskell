data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

instance Foldable Tree where
  -- fold :: Monoid m => Tree m -> m
  -- fold Leaf = mempty
  -- fold (Node l c r) = fold l `mappend` c `mappend` fold r

  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap f Leaf = mempty
  foldMap f (Node l c r) = foldMap f l <> f c <> foldMap f r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z Leaf = z
  foldr f z (Node l c r) = foldr f (c `f` foldr f z r) l

  foldl :: (b -> a -> b) -> b -> Tree a -> b
  foldl f z Leaf = z
  foldl f z (Node l c r) = foldl f (foldl f z l `f` c) r

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap g Leaf = Leaf
  fmap g (Node l c r) = Node (fmap g l) (g c) (fmap g r)

instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Node l c r) = pure Node <*> traverse f l <*> f c <*> traverse f r
