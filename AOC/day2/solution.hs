import Data.Bits (Bits (xor))
import System.IO (readFile')

main = do
  content <- readFile' "input.txt"
  let arrays = contentToArray content
  let safeArrays = map safeArray arrays
  let count = foldr (\x acc -> if x then acc + 1 else acc) 0 safeArrays
  print count

contentToArray :: String -> [[String]]
contentToArray content = map words (lines content)

safeArray (x : y : xs)
  | (read x :: Int) < (read y :: Int) = safeIncreasingArray (x : y : xs) 1 || (safeDecreasingArray (y : xs) 0 `xor` safeDecreasingArray (x : xs) 0)
  | (read x :: Int) > (read y :: Int) = safeDecreasingArray (x : y : xs) 1 || (safeIncreasingArray (y : xs) 0 `xor` safeIncreasingArray (x : xs) 0)
  | otherwise = False

safeIncreasingArray [x] _ = True
safeIncreasingArray (x : y : xs) fails
  | (read y - read x) `notElem` [1 .. 3] && fails == 0 = False
  | (read y - read x) `notElem` [1 .. 3] = safeIncreasingArray (y : xs) (fails - 1)
  | otherwise = safeIncreasingArray (y : xs) fails

safeDecreasingArray [x] _ = True
safeDecreasingArray (x : y : xs) fails
  | (read x - read y) `notElem` [1 .. 3] && fails == 0 = False
  | (read x - read y) `notElem` [1 .. 3] = safeDecreasingArray (y : xs) (fails - 1)
  | otherwise = safeDecreasingArray (y : xs) fails
