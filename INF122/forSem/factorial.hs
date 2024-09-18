module Factorial where

fac :: (Int -> Int)
fac =
  ( \x -> case () of
      value
        | x > 1 -> x * fac (x - 1)
        | otherwise -> 1
  )

fac2 :: Integer -> Integer
fac2 x
  | x > 1 = x * fac2 (x - 1)
  | otherwise = 1
