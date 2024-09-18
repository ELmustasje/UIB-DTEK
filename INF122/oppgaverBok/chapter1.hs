product2 :: [Int] -> Int
product2 (x : xs) = x * product (xs)

-- 4 qsort2
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort larger ++ [x] ++ qsort smaller
 where
  larger = [a | a <- xs, a >= x]
  smaller = [a | a <- xs, a < x]

-- 5 it would create an orders set
