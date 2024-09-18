module Week36 where

palindrome :: (Eq a) => [a] -> Bool
palindrome a = a == reverse a

third1, third2, third3 :: [a] -> a
third1 = head . tail . tail
third2 = (!! 2)
third3 (x : y : z : xs) = z

squares1, squares2 :: Int -> Int
squares1 x = sum [x ^ 2 | x <- [0 .. x]]
squares2 = (\x -> squares1 x)

pyths :: Int -> [(Int, Int, Int)]
pyths x = [(a, b, c) | a <- [1 .. x], b <- [1 .. x], c <- [1 .. x], a ^ 2 + b ^ 2 == c ^ 2]

luhnDouble :: Int -> Int
luhnDouble x
  | x * 2 <= 9 = x * 2
  | otherwise = (x * 2) - 9

luhnFixed :: Int -> Int -> Int -> Int -> Bool
luhnFixed a b c d = sum (map (luhnDouble) [a, b, c, d]) `mod` 10 == 0

luhnDoubleEveryOther :: [Int] -> [Int]
luhnDoubleEveryOther [] = []
luhnDoubleEveryOther [x] = [x]
luhnDoubleEveryOther (x : y : xs) = [x] ++ [luhnDouble y] ++ luhnDoubleEveryOther xs

luhn xs = (sum (luhnDoubleEveryOther (reverse xs))) `mod` 10 == 0
