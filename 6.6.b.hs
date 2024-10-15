concat :: [[a]] -> [a]
concat [] = []
concat ([]:xs) = Main.concat xs
concat ((x:xs):ys) = x : Main.concat (xs : ys)
