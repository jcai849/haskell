import Data.List

data Op = Add | Sub | Mul | Div | Exp
  deriving (Eq)

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 0 && y /= 1 && x `mod` y == 0
valid Exp x y = x /= 1 && y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x ^ y

data Expr = Val Int | App Op Expr Expr
  deriving (Eq)

instance Ord Expr where
  compare x y = compare (count_ops x) (count_ops y)

count_ops :: Expr -> Int
count_ops (Val _) = 0
count_ops (App _ el er) = 1 + count_ops el + count_ops er

instance Show Expr where
  show (Val n)     = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e       = "(" ++ show e ++ ")"
                      
values :: Expr -> [Int]
values (Val n)        = [n]
values (App _ l r)    = values l ++ values r

eval :: Expr -> [Int]
eval (Val n)          = [n | n > 0]
eval (App o l r)      = [apply o x y | x <- eval l, 
                                       y <- eval r, 
                                       valid o x y]
subs :: [a] -> [[a]]
subs []       = [[]]
subs (x:xs)   = yss ++ map (x:) yss
  where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x []     = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms []     = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs =  [x | y <- subs xs, x <- perms y]

split :: [a] -> [([a], [a])]
split []       = []
split [_]      = []
split (x:xs)   = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns  = [e | (ls, rs) <- split ns,
                 l        <- exprs ls,
                 r        <- exprs rs,
                 e        <- combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add,Sub,Mul,Div,Exp]

type Result = (Expr, Int)

results :: [Int] -> [Result]
results []  = []
results [n] = [(Val n, n) | n > 0]
results ns  = [res | (ls, rs) <- split ns,
                      ls      <- results ls,
                      ry      <- results rs,
                      res     <- combine' ls ry]

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) =
  [(App o l r, apply o x y) | o <- ops, valid o x y]

type ExprDist = Result

distance :: Int -> Result -> ExprDist
distance n (e, m) = (e, abs (m - n))

data ProximalExprs = ProximalExprs [Expr] Int
  deriving (Eq)

instance Show ProximalExprs where
  show (ProximalExprs es p) = "Distance: " ++ show p ++
                              "\nExpressions: " ++ show (sort es)

nearest_solutions :: [Int] -> Int -> ProximalExprs
nearest_solutions ns n =
   foldr accum_smaller (ProximalExprs [] 0)
     [distance n r | ns' <- choices ns, r <- results ns']

accum_smaller :: ExprDist -> ProximalExprs -> ProximalExprs
accum_smaller (e, d) (ProximalExprs es p)
  | null es || d < p  = ProximalExprs [e] d
  |            d == p = ProximalExprs (e:es) d
  |            d > p  = ProximalExprs es p

sols = nearest_solutions [1, 3, 7, 10, 25, 50] 765
