instance Eq a => Eq (Maybe a) where
  Nothing == Nothing = True
  Just x  == Just y  = True

instance Eq a => Eq [a] where
  []     == []     = True
  (x:xs) == (y:ys) | x == y    = xs == ys
                   | otherwise = False
  
