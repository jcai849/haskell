import Test.QuickCheck

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) | p x       = Main.dropWhile p xs
                   | otherwise = (x:xs)

prop_test_dropWhile :: Fun Int Bool -> [Int] -> Bool
prop_test_dropWhile (Fun _ p) xs
  = Main.dropWhile p xs == Prelude.dropWhile p xs

main :: IO ()
main = quickCheck prop_test_dropWhile
