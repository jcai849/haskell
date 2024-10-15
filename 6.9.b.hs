{-
Step 1: define the type

take :: Int -> [a] -> [a]

Step 2: enumerate the cases

take 0 _
take 1 (x:xs)
take n (x:xs)

Step 3: define the simple cases

take 0 _ = []
take 1 (x:xs) = [x]

Step 4: define the other cases

take n (x:xs) = x : take (n-1) xs

Step 5: generalise and simplify

take 1 unnecessary

-}

take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = x : Main.take (n-1) xs
