luhnDouble :: Int -> Int
luhnDouble x = if xx > 9 then xx - 9 else xx
               where xx = x * 2 
luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = (luhnDouble w + x + luhnDouble y + z) `mod` 10 == 0
