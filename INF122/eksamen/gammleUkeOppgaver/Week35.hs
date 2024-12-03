double :: [Int] -> [Int]
double = map (* 2)

sm1, sm2 :: [Int] -> Int
sm1 = foldr (+) 0
sm2 = foldl (+) 0

adds :: [(Int, Int)] -> [Int]
adds = map (\(x, y) -> x + y)
