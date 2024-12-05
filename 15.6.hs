sqroot :: Double -> Double
sqroot n = r
  where
    (_, r) = convergence
    convergence = head $ dropWhile converging (zip approx (tail approx))
    converging p = distance p > tolerance
    distance (x, y) = abs (x - y)
    tolerance = 0.00001
    approx = iterate next init
    init = 1.0
    next a = (a + n / a) / 2
