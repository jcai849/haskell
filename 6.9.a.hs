{-
Step 1: define the type

sum :: Num a => [a] -> a

Step 2: enumerate the cases

sum []
sum (x:xs)

Step 3: define the simple cases

sum [] = 0

Step 4: define the other cases

sum (x:xs) = x + sum xs

Step 5: generalise and simplify

?

-}

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + Main.sum xs
