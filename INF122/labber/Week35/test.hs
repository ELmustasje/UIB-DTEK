q1 :: Int -> Int
q1 = (+) 2

q2 :: Int -> Int
q2 = (+ 2)

q3 :: Int -> Int
q3 = (2 +)

q4 :: ([[Char]], Char)
q4 = (["foo", "bar"], 'a')

q6 :: [(Bool, [[Char]])]
q6 = [(True, []), (False, [['a']])]

q7 :: Int -> [b] -> b
q7 = \x y -> y !! x

q8 :: [Int -> [a] -> [a]]
q8 = [take, drop, \x y -> [y !! x]]
