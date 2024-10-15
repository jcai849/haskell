replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : Main.replicate (n-1) x
