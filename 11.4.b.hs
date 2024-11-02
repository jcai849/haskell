-- Tic-tac-toe example from chapter 11 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

-- Basic declarations

import Data.Char
import Data.List
import System.IO

type Grid = [[Player]]

data Player = O | B | X
              deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O

-- Grid utilities

empty :: Int -> Grid 
empty size = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
            os = length (filter (== O) ps)
            xs = length (filter (== X) ps)
            ps = concat g

wins :: Player -> Int -> Grid -> Bool
wins p n g = any (line p n) (rows ++ cols ++ dias)
           where
              rows = g
              cols = transpose g
              dias = [diag g, diag (map reverse g)]

line :: Player -> Int -> [Player] -> Bool
line p n l = sum playerMatches >= n
  where
    playerMatches = map (fromEnum . (==p)) l
  

diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..(length g)-1]]

won :: Int -> Grid -> Bool
won n g = wins O n g || wins X n g

-- Displaying a grid

putGrid :: Grid -> IO ()
putGrid g =
   putStrLn . unlines . concat . interleave bar . map showRow $ g
   where bar = [replicate ((size*4)-1) '-']
         size = length g

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
             beside = foldr1 (zipWith (++))
             bar    = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x []     = []
interleave x [y]    = [y]
interleave x (y:ys) = y : x : interleave x ys

-- Making a move

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B
  where size = length g

move:: Grid -> Int -> Player -> [Grid]
move g i p =
   if valid g i then [chop size (xs ++ [p] ++ ys)] else []
   where (xs,B:ys) = splitAt i (concat g)
         size = length g

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

-- Reading a natural number

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   xs <- getLine
                   if xs /= [] && all isDigit xs then
                      return (read xs)
                   else
                      do putStrLn "ERROR: Invalid number"
                         getNat prompt

-- Game trees

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Int -> Grid -> Player -> Tree Grid
gametree n g p = Node g [gametree n g' (next p) | g' <- moves n g p]

moves :: Int -> Grid -> Player -> [Grid]
moves n g p | won n g     = []
            | full g      = []
            | otherwise   = concat [move g i p | i <- [0..((size^2)-1)]]
              where
                size  = length g

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

-- Minimax

minimax :: Int -> Tree Grid -> Tree (Grid,Player)
minimax n (Node g [])
   | wins O n g  = Node (g,O) []
   | wins X n g  = Node (g,X) []
   | otherwise = Node (g,B) []
minimax n (Node g ts) 
   | turn g == O = Node (g, minimum ps) ts'
   | turn g == X = Node (g, maximum ps) ts'
                   where
                      ts' = map (minimax n) ts
                      ps  = [p | Node (_,p) _ <- ts']

bestmove :: Grid -> Int -> Player -> Grid
bestmove g n p = head [g' | Node (g',p') _ <- ts, p' == best]
               where 
                  tree = prune depth (gametree n g p)
                  Node (_,best) ts = minimax n tree

-- utils

prompt :: Player -> String
prompt p = "Player " ++ show p ++ ", enter your move: "

cls :: IO ()
cls = putStr "\ESC[2J"

goto :: (Int,Int) -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

-- Human vs computer

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          size <- selectSize
          win_length <- selectWinLength size
          play (empty size) win_length O

selectSize :: IO Int
selectSize = do
  s <- getNat "Select board size: "
  return s

selectWinLength :: Int -> IO Int
selectWinLength size = do
  i <- getNat "Select win length: "
  if i > size then do
    putStrLn "Win length larger than board!"
    s <- selectWinLength size
    return s
  else
    return i


play :: Grid -> Int -> Player -> IO ()
play g n p = do cls
                goto (1,1)
                putGrid g
                play' g n p

play' :: Grid ->  Int -> Player -> IO ()
play' g n p
   | wins O n g = putStrLn "Player O wins!\n"
   | wins X n g = putStrLn "Player X wins!\n"
   | full g   = putStrLn "It's a draw!\n"
   | p == O   = do i <- getNat (prompt p)
                   case move g i p of
                      []   -> do putStrLn "ERROR: Invalid move"
                                 play' g n p
                      [g'] -> play g' n (next p)
   | p == X   = do putStr "Player X is thinking... "
                   (play $! (bestmove g n p)) n (next p)
