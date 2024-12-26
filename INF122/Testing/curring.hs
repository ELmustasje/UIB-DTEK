test = curry (\(x, y) -> x + y) 1 2
test1 = uncurry (\x y -> x + y) (1, 2)
