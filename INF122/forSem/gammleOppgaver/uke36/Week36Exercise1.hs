module Week36Exercise1 where

f :: [Integer] -> [t] -> [(Integer, t)]
f xs ys = zip (reverse xs) ys
