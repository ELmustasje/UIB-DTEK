import Data.List
import System.Exit
import System.IO

mengde [] = True
mengde (x : xs)
  | x `elem` xs = False
  | otherwise = mengde xs

rep [] = []
rep (x : xs) = if x `elem` xs then rep xs else x : rep xs

del [] _ = True
del (x : xs) ys = if x `elem` ys then del xs ys else False

eq xs ys = del xs ys && del ys xs

eqG f xs ys = foldr (\x acc -> (aux f x ys) && acc) True xs
 where
  aux f x ys = foldr (\y acc -> f x y || acc) False ys

ps xs = pl (rep xs)
pl [] = [[]]
pl (x : xs) = [s | r <- pl xs, s <- [x : r, r]]

main :: IO ()
main = do
  input <- getLine
  process $ words input

process [] = exitSuccess
process (x : xs)
  | x == "L" = processFile xs
  | otherwise = processWords (x : xs)

processFile (x : xs) = do
  fileContent <- readFile' x
  let wordList = words fileContent
  processWords wordList

processWords :: [String] -> IO ()
processWords xs = do
  let msg = if (mengde xs) then (show $ ps xs) else "Ikke mengde"
  putStrLn msg
  main
