import Test.QuickCheck
import Test.QuickCheck.Function

prop_mf_equiv_lc :: [Int] -> Fun Int Bool -> Fun Int Int -> Bool
prop_mf_equiv_lc xs (Fun _ p) (Fun _ f) =
  map f (filter p xs) == [f x | x <- xs, p x]

main :: IO ()
main = quickCheck prop_filterMap
