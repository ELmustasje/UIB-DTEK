module HigherOrderFunctions where

app :: (Int -> Int) -> Int -> Int
app f = f

add1 :: Int -> Int
add1 x = x + 1

-- map
mapTest :: [Int]
mapTest = map (\x -> x * 2) [1, 2, 3, 4, 5]

-- filter
filterTest :: [Int]
filterTest = filter (\x -> x `mod` 2 /= 0) [1, 2, 3, 4, 5, 6, 8, 9]
