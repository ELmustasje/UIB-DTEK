abc :: t -> Int -> t -> [t]
abc x n y = [x | a <- [1 .. n]] ++ [y]

tryggIndex :: [t] -> Int -> t -> t
tryggIndex xs n def
  | n < length xs = xs !! n
  | otherwise = def

inds :: String -> String -> [Int]
inds _ "" = []
inds comp str = aux str comp [] 0
 where
  aux str comp inds cind
    | take (length comp) str == comp = aux (tail str) comp (inds ++ [cind]) (cind + 1)
    | (length comp) > (length str) = inds
    | otherwise = aux (tail str) comp inds (cind + 1)

splitToWords :: String -> [String]
splitToWords str = aux str [] ""
 where
  aux (c : str) words cWord
    | c == " " = aux str (words ++ [cWord]) ""
    | null (c : str) = words
    | otherwise = aux str words (cWord ++ c)
