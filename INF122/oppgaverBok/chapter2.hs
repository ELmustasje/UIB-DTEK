-- 3
n = a `div` length xs
 where
  a = 10
  xs = [1, 2, 3, 4, 5]

last2 :: [a] -> a
last2 (x : xs)
  | length (x : xs) == 1 = x
  | otherwise = last2 (xs)

init2 :: [a] -> [a]
init2 xs = take (length xs - 1) xs

init3 :: [Int] -> [Int]
init3 [_] = []
init3 (x : xs) = [x] ++ init3 xs
