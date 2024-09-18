module Week37Exercise1 where

-- faster
semiFermat :: Integer -> Integer -> [(Integer, Integer, Integer)]
semiFermat n m = [(a, b, c) | a <- [1 .. n - 2], b <- [a .. n - 1], c <- [b .. n], a ^ m + b ^ m == c ^ (m - 1)]
