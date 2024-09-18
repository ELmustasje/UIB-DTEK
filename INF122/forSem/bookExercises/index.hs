index :: [a] -> Int -> a
index xs 0 = head xs
index xs n = index (tail xs) (n - 1)
