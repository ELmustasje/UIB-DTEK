module Week36Exercise2 where

import Data.Maybe

semiRepetitive :: String -> Maybe String
semiRepetitive word
  | firstHalf == secondHalf = Just firstHalf
  | otherwise = Nothing
  where
    midpoint = length word `div` 2
    firstHalf = take midpoint word
    secondHalf = drop (midpoint + (length word `mod` 2)) word

getMiddleOfString :: String -> Maybe Char
getMiddleOfString word
  | length word `mod` 2 == 1 = Just (word !! (length word `div` 2))
  | otherwise = Nothing

decomposeSemiRepetitive :: String -> Maybe (String, Maybe Char)
decomposeSemiRepetitive word
  | firstHalf == secondHalf = Just (firstHalf, getMiddleOfString word)
  | otherwise = Nothing
  where
    midpoint = length word `div` 2
    firstHalf = take midpoint word
    secondHalf = drop (midpoint + (length word `mod` 2)) word

createSemiRepetitive :: String -> Maybe Char -> String
createSemiRepetitive word char
  | isNothing char = word ++ word
  | isJust char = word ++ [fromJust char] ++ word
