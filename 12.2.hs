instance Functor ((->) a) where
  -- fmap :: (a -> b) -> (c -> a) -> (c -> b)
  fmap = (.)
