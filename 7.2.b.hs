import Test.QuickCheck

-- N.B. Type signature in book not compiling, altered here

any :: (a -> Bool) -> [a] -> Bool
any p = or . (map p)

prop_test_any :: Fun Int Bool -> [Int] -> Bool
prop_test_any (Fun _ p) xs = Main.any p xs == Prelude.any p xs

main :: IO ()
main = quickCheck prop_test_any
