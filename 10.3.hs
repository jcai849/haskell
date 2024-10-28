type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard b = sequence_ [putRow row num | (row, num) <- zip [1..] b]

main :: IO ()
main = putBoard ([1..10] ++ reverse [1..9])
