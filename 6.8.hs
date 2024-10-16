import Test.QuickCheck
import Data.List (sort)

merge :: Ord a => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | x >=  y = y : merge (x:xs) ys
merge []  y = y
merge x  [] = x

halve :: [a] -> ([a],[a])
halve x = (take (l `div` 2) x, drop (l `div` 2) x)
          where l = length x

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort a) (msort b) where (a,b) = halve xs

prop_sorted_msort :: [Int] -> Bool
prop_sorted_msort xs = msort xs == sort xs

main :: IO ()
main = quickCheck prop_sorted_msort
