(!!) :: [a] -> Int -> a
(x:_) !! 0 = x
(_:xs) !! n = xs Main.!! (n-1)
