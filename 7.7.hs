import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int

bin2int bits = sum [w*b | (w,b) <- zip weights bits]
               where weights = iterate (*2) 1

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bin . ord)

chopn :: Int -> [Bit] -> [[Bit]]
chopn _ [] = []
chopn n bits = take n bits : chopn n (drop n bits)

chop8 = chopn 8

decode :: [Bit] -> String
decode = map (chr . bin2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

-----------------------------------------------------

parity_bit :: [Bit] -> Bit
parity_bit xs = sum xs `mod` 2

add_parity :: [Bit] -> [Bit]
add_parity xs = xs ++ [parity_bit xs]

encode_w_parity :: String -> [Bit]
encode_w_parity = concat . map (add_parity . make8 . int2bin . ord)

chop9 = chopn 9

parity_valid :: [Bit] -> Bool
parity_valid xs = (parity_bit . init) xs == last xs

rm_parity :: [Bit] -> [Bit]
rm_parity xs | parity_valid xs = init xs
             | otherwise       = error "Parity bit incorrect"

decode_w_parity :: [Bit] -> String
decode_w_parity = map (chr . bin2int . rm_parity) . chop9

transmit_w_parity :: String -> String
transmit_w_parity = decode_w_parity . channel . encode_w_parity
