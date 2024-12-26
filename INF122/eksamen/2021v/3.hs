data Tre = Leaf Int | Bransh Tre Int Tre deriving (Eq, Show)

foldt :: (Int -> a -> a) -> a -> Tre -> a
foldt f acc (Leaf n) = f n acc
foldt f acc (Bransh left n right) = foldt f (foldt f (f n acc) left) right

a = foldl (-) 1 [2, 3, 4]
