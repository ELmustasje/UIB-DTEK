import Data.Char
import System.IO (readFile')

main = do
  content <- readFile' "input.txt"
  let pairs = extractIntPairs content True
  let sum = foldr (\x acc -> acc + (fst x * snd x)) 0 pairs
  print sum

extractIntPairs :: String -> Bool -> [(Int, Int)]
extractIntPairs [] _ = []
extractIntPairs (x : xs) active
  | take 7 (x : xs) == "don't()" = extractIntPairs xs False
  | take 4 (x : xs) == "do()" = extractIntPairs xs True
  | active == False = extractIntPairs xs active
  | take 4 (x : xs) == "mul(" = findNumbers (drop 4 (x : xs)) : extractIntPairs (drop 4 (x : xs)) active
  | otherwise = extractIntPairs xs active

findNumbers xs = acc xs "" "" 1
 where
  acc (x : xs) intA intB aOrB
    | isDigit x = if aOrB == 1 then acc xs (intA ++ [x]) intB aOrB else acc xs intA (intB ++ [x]) aOrB
    | x == ')' = (read intA, read intB)
    | x == ',' = acc xs intA intB (aOrB * (-1))
    | otherwise = (0, 0)
