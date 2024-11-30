instance Monoid b => Monoid (a -> b) where
  -- mempty :: (a -> b)
  mempty = const mempty

  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
  f1 `mappend` f2  = \a -> f1 a `mappend` f2 a
