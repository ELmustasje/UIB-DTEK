factorial :: Int -> [Int]
factorial n = [x | x <- [0 .. 100], x `mod` n == 0]

sumOfSquares :: [Integer] -> Integer
sumOfSquares xs = sum [x ^ 2 | x <- xs]

sumOfSquares2 :: [Integer] -> Integer
sumOfSquares2 xs = sum (map (^ 2) xs)

duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x : xs) = [x, x] ++ duplicate xs

hasLength :: Int -> [a] -> Bool
hasLength n xs = n == length xs
