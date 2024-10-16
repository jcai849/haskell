import Test.QuickCheck

-- N.B. Type signature in book not compiling, altered here

all :: (a -> Bool) -> [a] -> Bool
all p xs = foldr (&&) True (map p xs)

prop_test_all :: Fun Int Bool -> [Int] -> Bool
prop_test_all (Fun _ p) xs = Main.all p xs == Prelude.all p xs

main :: IO ()
main = quickCheck prop_test_all
