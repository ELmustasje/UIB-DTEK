concats :: [[a]] -> [a]
concats [] = []
concats (a : as) = a ++ concats as
