instance Monad ((->) a) where
 -- (>>=) :: (m -> a) -> (a -> (m -> b)) -> (m -> b)
 (>>=) mx f m = f (mx m) m
