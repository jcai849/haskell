import Test.QuickCheck

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) | p x       = x : (Main.takeWhile p xs)
                   | otherwise = []

prop_test_takeWhile :: Fun Int Bool -> [Int] -> Bool
prop_test_takeWhile (Fun _ p) xs
  = Main.takeWhile p xs == Prelude.takeWhile p xs

main :: IO ()
main = quickCheck prop_test_takeWhile
