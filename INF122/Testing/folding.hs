sumF xs = foldr (+) 0 xs
andF xs = foldr (&&) True xs
orF xs = foldr (||) False xs
countF xs = foldr (\x acc -> acc + 1) 0 xs
isAllF xs e = foldr (\x acc -> x == e && acc) True xs
mapF f xs = foldr (\x acc -> (f x) : acc) [] xs
