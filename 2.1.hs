{-
ghci> 2+3*4
14
ghci> (2+3)*4
20
ghci> sqrt (3^2 + 4^2)
5.0
ghci> [1,2,3,4,5]
[1,2,3,4,5]
ghci> head [1,2,3,4,5]
1
ghci> tail [1..5]
[2,3,4,5]
ghci> [1..5] !! 2
3
ghci> take 3

<interactive>:8:1: error: [GHC-39999]
    * No instance for `Show ([a0] -> [a0])'
        arising from a use of `print'
        (maybe you haven't applied a function to enough arguments?)
    * In a stmt of an interactive GHCi command: print it
ghci> take 3 [1..5]
[1,2,3]
ghci> drop 3 [1..5]
[4,5]
ghci> length [1..5]
5
ghci> length [1..500000000000]
^CInterrupted.
ghci> length [1..5000]
5000
ghci> sum [1..5]
15
ghci> product [1..5]
120
ghci> [1,2,3] ++ [4,5]
[1,2,3,4,5]
ghci> reverse [1,2,3,4,5]
[5,4,3,2,1]

ghci> double x = x + x
ghci> quadruple x = double (double x)
ghci> quadruple 10
40
ghci> take (double 2) [1..5]
[1,2,3,4]
ghci> factorial n = product [1..n]
ghci> average ns = sum ns `div` length ns
ghci> average [1..10]
5

-}
