import Data.Char

minCaseBound c
  | isLower c = 'a'
  | isUpper c = 'A'

-- clb: case lower bound ('a' or 'A' in English)

letcase2int :: Char -> Char -> Int
letcase2int clb c = ord c - ord clb

int2letcase :: Char -> Int -> Char
int2letcase clb n = chr (ord clb + n)

shift :: Int -> Char -> Char
shift n c
  | isLetter c = int2let ((let2int c + n) `mod` 26)
  | otherwise  = c
  where 
    lower_bound = minCaseBound c
    int2let     = int2letcase lower_bound
    let2int     = letcase2int lower_bound

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]

table :: [Float]

table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

letters :: String -> Int
letters xs = length [x | x <- xs, isLetter x]

uncasedcount :: Char -> String -> Int
uncasedcount x xs = length [x' | x' <- xs, toLower x == toLower x']

freqs :: String -> [Float]
freqs xs = [percent (uncasedcount x xs) n | x <- ['a'..'z']]
           where n = letters xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o-e)^2)/e | (o,e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

crack :: String -> String
crack xs = encode (-factor) xs
  where
    factor = head (positions (minimum chitab) chitab)
    chitab = [chisqr (rotate n table') table | n <- [0..25]]
    table' = freqs xs
