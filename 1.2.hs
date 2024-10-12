import Test.QuickCheck

prop_sum_identity :: Int -> Bool
prop_sum_identity x = sum [x] == x

main :: IO ()
main = quickCheck prop_sum_identity
