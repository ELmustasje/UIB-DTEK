module Week34 where

isEmpty :: [Int] -> Bool
isEmpty xs = xs == []

safeHead :: [Int] -> [Int]
safeHead [] = []
safeHead (x : xs) = [x]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let lower = quickSort [y | y <- xs, y <= x]
      upper = quickSort [y | y <- xs, y > x]
   in lower ++ [x] ++ upper
