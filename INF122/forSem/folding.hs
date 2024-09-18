func1 = (\a -> foldr a 1 [1 .. 10]) -- dose opperator a on all elements in list
func2 = foldr (+) 10 [0, 2 .. 10] -- sums all nums in list
func3 = foldr (\x acc -> if x `mod` 2 == 0 then acc + 1 else acc) 0 [0 .. 10] -- counts even numbers in list
func4 = foldr (\x acc -> 1 == x && acc) True [1, 1, 1, 1, 1] -- checks if all elements is 1
func5 = (\e -> foldr (\x acc -> x == e && acc) True [1, 1]) -- checks if all elements is e
func6 = foldr (\x count -> count + 1) 0 [1, 2, 3, 4, 5]
