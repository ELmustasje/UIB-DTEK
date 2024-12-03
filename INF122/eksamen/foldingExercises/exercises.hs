evensR xs = foldr (\x acc -> if even x then x : acc else acc) [] xs
evensL xs = foldl (\acc x -> if even x then x : acc else acc) [] xs
oddsL xs = foldl (\acc x -> if odd x then x : acc else acc) [] xs
oddsR xs = foldr (\x acc -> if odd x then x : acc else acc) [] xs

countR e xs = foldr (\x acc -> if x == e then acc + 1 else acc) 0 xs
countL e xs = foldl (\acc x -> if x == e then acc + 1 else acc) 0 xs

isAllSameR xs = foldr (\x acc -> x == head xs && acc) True xs
isAllSameL xs = foldl (\acc x -> x == head xs && acc) True xs

isAllER xs e = foldr (\x acc -> x == e && acc) True xs
isAllEL xs e = foldl (\acc x -> x == e && acc) True xs

length xs = foldr (\x acc -> acc + 1) 0 xs

map' f = foldr (\x acc -> (f x) : acc) []

rev, rev2 :: [a] -> [a]
rev = foldl (\acc x -> x : acc) []
rev2 = foldr (\x acc -> acc ++ [x]) []

prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : (map ((:) x) acc)) []
