module ListExersices where

asc :: Int -> Int -> [Int]
asc l u
  | l > u = []
  | l == u = [l]
  | l < u = l : asc (l + 1) u

elem1 :: (Eq a) => a -> [a] -> Bool
elem1 _ [] = False
elem1 e (x : xs) = (e == x) || elem1 e xs

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs)
  | x `elem1` xs = nub xs
  | otherwise = x : nub xs

isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [a] = True
isAsc (x : xs) = (x <= head xs) && (isAsc xs)
