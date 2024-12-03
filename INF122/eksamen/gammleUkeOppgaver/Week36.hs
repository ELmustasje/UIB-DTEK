palindrome :: (Eq a) => [a] -> Bool
palindrome a = a == reverse a

third1, third2, third3 :: [a] -> a
third1 xs = head $ tail $ tail xs
third2 xs = xs !! 2
third3 (x : y : z : xs) = z

squares1, squares2 :: Int -> Int
squares1 n = sum [x * x | x <- [0 .. n]]
squares2 = (\x -> squares1 x)

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], a * a + b * b == c * c]

luhnDouble :: Int -> Int
luhnDouble x
  | a > 9 = a - 9
  | otherwise = a
 where
  a = x * 2

luhnFixed :: Int -> Int -> Int -> Int -> Bool
luhnFixed a b c d = x `mod` 10 == 0
 where
  x = sum [luhnDouble a, b, luhnDouble c, d]

luhn :: [Int] -> Bool
luhn xs = sum (aux (reverse xs) 0) `mod` 10 == 0
 where
  aux [] _ = []
  aux (y : ys) n
    | n `mod` 2 == 0 = y : aux ys (n + 1)
    | otherwise = luhnDouble y : aux ys (n + 1)
