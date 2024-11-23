data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
  deriving (Show)

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var a) = Var (g a)
  fmap _ (Val int) = Val int
  fmap g (Add a b) = Add (fmap g a) (fmap g b)

instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  Var f <*> Var x = Var (f x)
  Var f <*> Val int = Val int
  Var f <*> (Add a b) = Add (Var f <*> a) (Var f <*> b)
  Val int <*> _ = Val int
  (Add a b) <*> Var x = Add (a <*> Var x) (b <*> Var x)
  (Add a b) <*> Val int = Val int
  -- Bearing equivalence to List application:
  (Add a b) <*> (Add c d) = Add (Add (a <*> c) (a <*> d)) (Add (b <*> c) (b <*> d))

-- Passes each Var in the Expr given by the first argument to
-- the second argument, substituting the result in-place.
instance Monad Expr where
  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  Var a >>= f = f a
  Val int >>= _ = Val int
  Add a b >>= f = Add (a >>= f) (b >>= f)

m1 = Var (const "y")
m2 = Add (Var "a") (Add (Var "b") (Val 1))
-- m1 <*> m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (x1 x2)))
