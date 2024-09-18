module Week37 where

import Data.Char
import Prelude hiding (and, concat, elem, replicate, (!!))

euclid :: Int -> Int -> Int
euclid a b
  | a == b = a
  | a > b = euclid (a - b) b
  | otherwise = euclid a (b - a)

and :: [Bool] -> Bool
and [] = True
and (x : xs)
  | x == False = False
  | otherwise = and xs

concat :: [[a]] -> [a]
concat [] = []
concat [[a]] = [a]
concat (x : xs) = x ++ concat xs

replicate :: Int -> a -> [a]
replicate 0 a = []
replicate x a = [a] ++ replicate (x - 1) a

(!!) :: [a] -> Int -> a
(!!) (x : xs) 0 = x
(!!) (x : xs) index = (!!) xs (index - 1)

elem :: (Eq a) => a -> [a] -> Bool
elem a [] = False
elem a (x : xs)
  | a == x = True
  | otherwise = elem a xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x : xs) (y : ys)
  | x <= y = [x] ++ merge xs (y : ys)
  | y < x = [y] ++ merge (x : xs) ys

half :: [a] -> ([a], [a])
half xs = ((take ((length xs) `div` 2) xs), (drop (((length xs) `div` 2)) xs))

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort (fst (half xs))) (msort (snd (half xs)))

tokenise :: String -> [String]
tokenise = go []
 where
  -- Hjelpefunksjon som tar en akkumulert liste og en gjenværende streng
  go acc [] = reverse acc
  go acc (c : cs)
    | c `elem` "+-*/()" = go ([c] : acc) cs
    | isDigit c =
        let (digits, rest) = span isDigit (c : cs)
         in go (digits : acc) rest
    | isAlpha c =
        let (letters, rest) = span isAlpha (c : cs)
         in go (letters : acc) rest
    | otherwise = go acc cs
