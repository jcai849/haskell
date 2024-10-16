import Test.QuickCheck

map :: (a -> b) -> [a] -> [b]
map f = foldr ((:) . f) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x y -> if p x then x : y else y) []

prop_map_equiv :: Fun Int Int -> [Int] -> Bool
prop_map_equiv (Fun _ f) xs = Main.map f xs == Prelude.map f xs

prop_filter_equiv :: Fun Int Bool -> [Int] -> Bool
prop_filter_equiv (Fun _ p) xs = Main.filter p xs == Prelude.filter p xs

main :: IO ()
main = do
  quickCheck prop_map_equiv
  quickCheck prop_filter_equiv
