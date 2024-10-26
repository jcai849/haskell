import Data.List (findIndices)

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice []  _  = True
isChoice (x:xs) ys | elem x ys = isChoice xs ys
                   | otherwise = False
