luhnDouble :: Int -> Int
luhnDouble x | xx > 9    = xx - 9 
             | otherwise = xx
               where xx  = x * 2 

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0
