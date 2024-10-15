and :: [Bool] -> Bool
and (False:_) = False
and [True] = True
and (True:xs) = Main.and xs
