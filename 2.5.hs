{-
init' [x] = []
init' (x:xs) = [x] ++ init' xs
-}

-- init' xs =  reverse (tail (reverse xs))

-- init' xs = take (length xs - 1) xs
