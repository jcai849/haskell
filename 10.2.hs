type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard :: Board -> IO ()
putBoard b = iterBoard 1 b
  where
    iterBoard _ [] = return ()
    iterBoard row (x:xs) = do
      putRow row x
      iterBoard (row + 1) xs

main :: IO ()
main = putBoard ([1..10] ++ reverse [1..9])
