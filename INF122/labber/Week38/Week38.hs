module Week38 where

remg :: [a] -> (a -> Bool) -> [a]
remg [] _ = []
remg (x : xs) p
  | p x = xs
  | otherwise = x : remg xs p

mapF :: (a -> b) -> [a] -> [b]
mapF f = foldr (\x acc -> f x : acc) []

and1, and2 :: [Bool] -> Bool
and1 = foldr (&&) True
and2 = foldl (&&) True

counterExample :: [Bool]
counterExample = repeat False

-- 7.1
mapFilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapFilter m f xs = map m (filter f xs)

-- 7.4
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0

-- 7.9
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = [] -- Hvis listen er tom, returner en tom liste
altMap f1 f2 (x : xs) = f1 x : altMap f2 f1 xs -- Anvend f1 på første element, f2 på neste, og bytt mellom dem

-- 7.10
luhnDouble :: Int -> Int
luhnDouble x = if doubled > 9 then doubled - 9 else doubled
 where
  doubled = x * 2

-- Luhn implementation
luhn :: [Int] -> Bool
luhn xs = sum (altMap id luhnDouble (reverse xs)) `mod` 10 == 0
