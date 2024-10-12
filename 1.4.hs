qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ smaller
               where
                  larger = [a | a <- xs, a > x]
                  smaller = [b | b <- xs, b <= x]
