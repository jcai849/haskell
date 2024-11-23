{-
x :: f a
pure id <*> x = x

g :: a -> b
x :: a
pure (g x) = pure g <*> pure x

x :: f (a -> b)
y :: a
x <*> pure y = pure (\g -> g y) <*> x

x :: f (b -> c)
y :: f (a -> b)
z :: f a

x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-}
