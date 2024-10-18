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

chopn :: Int -> [Bit] -> [[Bit]]
chopn _ [] = []
chopn n bits = take n bits : chopn n (drop n bits)

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

-----------------------------------------------------

faulty_channel :: [Bit] -> [Bit]
faulty_channel = tail

faulty_transmit :: String -> String
faulty_transmit = decode_w_parity . faulty_channel . encode_w_parity

{-

	ghci> faulty_transmit "a"
	"*** Exception: Parity bit incorrect
	CallStack (from HasCallStack):
	  error, called at 7.8.hs:39:34 in main:Main
	ghci> faulty_transmit "b"
	"1"
	ghci> faulty_transmit "c"
	"*** Exception: Parity bit incorrect
	CallStack (from HasCallStack):
	  error, called at 7.8.hs:39:34 in main:Main
	ghci> faulty_transmit "d"
	"2"

-}
