data Expr = Val Int | Add Expr Expr | Mult Expr Expr

type Cont = [Op]

data Op = EVAL_ADD Expr | ADD Int | EVAL_MULT Expr | MULT Int

eval :: Expr -> Cont -> Int
eval (Val n)    c = exec c n
eval (Mult x y) c = eval x (EVAL_MULT y : c)
eval (Add x y)  c = eval x (EVAL_ADD y : c)

exec :: Cont -> Int -> Int
exec []                n = n
exec (EVAL_ADD y : c)  n = eval y (ADD n : c)
exec (EVAL_MULT y : c) n = eval y (MULT n : c)
exec (MULT n : c) m = exec c (n*m)
exec (ADD  n : c) m = exec c (n+m)

value :: Expr -> Int
value e = eval e []

-- (2 + 3) + 4
v = value (Add (Add (Val 2) (Val 3)) (Val 4))

-- (2 + 3) * 4
v' = value (Mult (Add (Val 2) (Val 3)) (Val 4))
