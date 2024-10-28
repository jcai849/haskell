adder :: IO ()
adder = do
  putStr "How many numbers? "
  input <- getLine
  ns <- sumNums (read input :: Int) 0
  putStrLn ("The total is " ++ show ns)

sumNums :: Int -> Int -> IO Int
sumNums 0 x = return x
sumNums n x = do
  input <- getLine
  sumNums (n-1) ((read input :: Int) + x)
