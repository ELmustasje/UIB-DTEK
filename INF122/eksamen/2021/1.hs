import Data.Char (digitToInt)
import Data.List (group, sort)

abc :: t -> Int -> t -> [t]
abc h c e = [h | x <- [1 .. c]] ++ [e]

tryggIndex :: [t] -> Int -> t -> t
tryggIndex xs indx def
  | indx >= length xs = def
  | otherwise = xs !! indx

inds :: String -> String -> [Int]
inds xs element = aux xs 0 []
 where
  aux [] _ arr = arr
  aux (x : xs) currentIndex arr
    | take (length element) (x : xs) == element = aux xs (currentIndex + 1) (arr ++ [currentIndex])
    | otherwise = aux xs (currentIndex + 1) arr

ordTeller str = aux (group $ sort $ words str)
 where
  aux wordList = [(head word, length word) | word <- wordList]

dec :: Int -> String -> Int
dec base str = foldl (\acc x -> acc * base + read [x]) 0 str
