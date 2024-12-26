finnFiksPunkt :: (Eq a) => (a -> a) -> a -> Int -> Maybe a
finnFiksPunkt f x 0 = Nothing
finnFiksPunkt f x n = if f x == x then Just x else finnFiksPunkt f (f x) (n - 1)

serie :: (Int -> b) -> Int -> [b]
serie f i = foldr (\x acc -> [f x] ++ acc) [] [i ..]

sq :: Int -> [Int]
sq n = take n $ serie (^ 2) 0

streng :: Int -> String
streng n = unwords $ take n $ serie (\f -> "f(" ++ show f ++ "),") 0

h :: (Int -> Int) -> Int -> [Int]
h f i = foldr (\x acc -> [foldr (\x acc -> acc + f x) 0 [0 .. x]] ++ acc) [] [i ..]

hva xs = foldr (++) [] (map sing xs) where sing x = [x]

a = filter (> 0) . map (+ 1)
b = map (+ 1) . filter (>= 0)
