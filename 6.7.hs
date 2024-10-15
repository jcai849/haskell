merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | x >  y = y : merge (x:xs) ys
merge []  y = y
merge x  [] = x
