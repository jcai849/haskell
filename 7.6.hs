
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)





{-
chop8 :: [Bit] -> [[Bit]]
chop8 [] = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)
-}

type Bit = Int
chop8 :: [Bit] -> [[Bit]]
chop8 = unfold (==[]) (take 8) (drop 8)

{-
map :: (a -> b) -> [a] -> [b]
map f = foldr (\x y -> f x : y) []
-}

map :: (a -> b) -> [a] -> [b]
map f = unfold null (f . head) (drop 1)

{-
iterate f x = [x, f x, f (f x), f (f (f x)), ...]
-}

iterate :: (a -> a) -> a -> [a]
iterate f = unfold (\_ -> False) id f
