par :: Int -> [(Int, Int)]
par n = [(x, y) | x <- [1 .. n], y <- [1 .. n]]

sm n = sum [x * y | (x, y) <- par n]

pyt n = [(a, b, c) | a <- [1 .. n], b <- [1 .. n], c <- [1 .. n], a ^ 2 + b ^ 2 == c ^ 2]

sk xs ys = sum $ [x * y | (x, y) <- zip xs ys]

re n x = [x | n <- [1 .. n]]
