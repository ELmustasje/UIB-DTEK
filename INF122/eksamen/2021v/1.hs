takeW n str = unwords $ take n (words str)

erSubListe :: (Eq a) => [a] -> [a] -> Bool
erSubListe [] _ = True
erSubListe _ [] = False
erSubListe (x : xs) (y : ys)
  | x == y = erSubListe xs ys
  | otherwise = erSubListe (x : xs) ys

erSubStreng _ [] = False
erSubStreng xs (y : ys)
  | head xs == y = if take (length xs) (y : ys) == xs then True else erSubStreng xs ys
  | otherwise = erSubStreng xs ys
