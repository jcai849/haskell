{-
The recursive version of fibonacci will never reach the base case, and iterate forever. An improvement is given below.
-}

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n | n > 0 = fib (n-2) + fib (n-1)
