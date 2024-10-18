altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f1 f2 []     = []
altMap f1 f2 (x:xs) = f1 x : altMap f2 f1 xs

luhnDouble :: Int -> Int
luhnDouble x | xx > 9    = xx - 9
             | otherwise = xx
               where xx  = x * 2

luhnDigits :: [Int] -> [Int]
luhnDigits xs | even . length $ xs = altMap luhnDouble id xs
              | otherwise          = altMap id luhnDouble xs

luhn :: [Int] -> Bool
luhn xs = (sum . luhnDigits) xs `mod` 10 == 0
