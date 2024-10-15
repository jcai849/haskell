elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem a (x:xs) | a == x = True
              | a /= x = Main.elem a xs
