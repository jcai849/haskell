instance Applicative ((->) a) where
  -- pure :: a -> (b -> a)
  -- pure :: a -> (b -> a)
  -- pure :: a -> b -> a
  pure x b = x

  -- <*> :: (f -> (a -> b)) -> (f -> a) -> (f -> b)
  (<*>) = ($)
