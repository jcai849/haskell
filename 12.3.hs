instance Applicative ((->) a) where
  -- pure :: a -> (f -> a)
  -- pure :: a -> f -> a
  pure a _ = a

  -- (<*>) :: (f (a -> b)) -> f a -> f b
  -- (<*>) :: (f -> (a -> b)) -> (f -> a) -> (f -> b)
  -- (<*>) :: (f -> (a -> b)) -> (f -> a) -> f -> b
  (<*>) x y f = (x f)(y f)
