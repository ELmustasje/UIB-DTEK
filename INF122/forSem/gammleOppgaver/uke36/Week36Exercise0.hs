module Week36Exercise0 where

f :: String -> Char -> Bool
f word letter = not (not (letter `elem` word))
