harEl f xs = foldr (\x acc -> f x || acc) False xs

el f [] = []
el f (x : xs)
  | f x = x
  | otherwise = el f xs

gRep f t [] = []
gRep f t (x : xs) = if f x then t : gRep f t xs else x : gRep f t xs

data BT = B Int | N BT Int BT

foldt f acc (B a) = f a acc
foldt f acc (N l a r) = foldt f (f a (foldt f acc r)) l

elt n tre = foldt (\x acc -> (x == n) || acc) False tre

toL tr = foldt (\x acc -> x : acc) [] tr

a = dup (N (B 1) 2 (N (B 3) 3 (B 0)))

dup tre = mengde $ toL tre

mengde [] = True
mengde (x : xs) = if not (x `elem` xs) then False else mengde xs
