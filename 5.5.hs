pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..10], z <- [1..10], x^2 + y^2 == z^2]
