instance Foldable Maybe where
  -- fold :: Monoid a => Maybe a -> a
  -- fold (Just a) = a
  -- fold Nothing = mempty

  -- foldMap :: Monoid b => (a -> b) -> t a -> b
  foldMap f (Just a) = f a
  foldMap _ Nothing = mempty

  -- foldr :: (a -> b -> b) -> b -> t a -> b
  foldr f z (Just a) = a `f` z
  foldr _ _ Nothing  = mempty

  -- foldl :: (a -> b -> a) -> a -> t b -> a
  foldl f z (Just a) = z `f` a
  foldl _ _ Nothing  = mempty

instance Traversable Maybe where
  -- traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
  traverse _ Nothing  = pure Nothing
  traverse f (Just a) = Just <$> f a
