module Currying where

add :: Int -> Int -> Int
-- add x y = x + y
-- add x = (\y -> x + y)
add = (\x -> (\y -> x + y))

doubleList :: [Int] -> [Int]
doubleList = map (\x -> x * 2)
