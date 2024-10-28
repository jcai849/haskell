adder :: IO ()
adder = do
  putStr "How many numbers? "
  input <- getLine
  ns <- sequence $ replicate (read input :: Int) getLine
  putStrLn ("The total is " ++ show (sum (map read ns :: [Int])))
