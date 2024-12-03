import Data.List
import System.IO (readFile')

main = do
  content <- readFile' "input.in"
  let arrays = contentToArray content
  let diff = diffMinMax (fst arrays) (snd arrays)
  let sim = similartScore (fst arrays) (snd arrays)
  print sim

contentToArray content = (listA, listB)
 where
  listA = foldr (\x acc -> [read $ head x] ++ acc) [] [words x | x <- lines content]
  listB = foldr (\x acc -> [read $ last x] ++ acc) [] [words x | x <- lines content]

diffMinMax :: [Int] -> [Int] -> Int
diffMinMax listA listB = foldr (\x acc -> acc + (abs $ fst x - snd x)) 0 (zip (sort listA) (sort listB))

count e = foldr (\x acc -> if x == e then acc + 1 else acc) 0

similartScore listA listB = foldr (\x acc -> acc + (x * count x listB)) 0 listA
