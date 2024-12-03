module Week37 where

import Distribution.Simple.Utils (xargs)

euclid :: Int -> Int -> Int
euclid a b
  | a `mod` b == 0 = min a b
  | a > b = euclid (a - b) b
  | b > a = euclid a (b - a)

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = x : merge xs (y : ys)
  | y < x = y : merge (x : xs) ys

half :: [a] -> ([a], [a])
half xs = ((take ((length xs) `div` 2) xs), (drop (((length xs) `div` 2)) xs))

msort :: (Ord a) => [a] -> [a]
msort xs
  | length xs > 1 = merge (msort (fst (half xs))) (msort (snd (half xs)))
  | otherwise = xs
