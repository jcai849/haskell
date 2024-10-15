{-
if the two numbers are equal, this number is the result; otherwise, the smaller number is subtracted from the larger, and the same process is then repeated. For example:

> euclid 6 27

3
-}

euclid :: Int -> Int -> Int
euclid m n
  | m == n = m
  | m > n  = euclid (m - n) n
  | m < n  = euclid  m (n - m)
