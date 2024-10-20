data Expr = Val Int | Add Expr Expr
            deriving Show

e = Add (Add (Val 1) (Val 2)) (Val 3)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val x)   = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)
