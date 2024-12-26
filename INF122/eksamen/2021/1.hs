import Data.List

abc :: t -> Int -> t -> [t]
abc x n y = [x | y <- [1 .. n]] ++ [y]

tryggIndex :: [t] -> Int -> t -> t
tryggIndex xs n def
  | n < length xs = xs !! n
  | otherwise = def

inds :: String -> String -> [Int]
inds as xs = aux (length as) xs 0
 where
  aux _ [] _ = []
  aux lenSub (x : xs) currentIndex
    | take lenSub (x : xs) == as = currentIndex : aux lenSub xs (currentIndex + 1)
    | otherwise = aux lenSub xs (currentIndex + 1)

ordTeller :: String -> [(String, Int)]
ordTeller str = aux (sort $ words str)
 where
  aux [] = []
  aux (x : xs) =
    (x, length $ takeWhile (== x) (x : xs))
      : aux (dropWhile (== x) (x : xs))

dec :: Int -> String -> Int
dec n str = aux n str (length str - 1)
 where
  aux _ [] _ = 0
  aux n (x : xs) i
    | n <= read [x] = error "for stort siffer"
    | otherwise = read [x] * (n ^ i) + aux n xs (i - 1)

type EtOrd = String
type Linje = [EtOrd]
type Side = [Linje]
type Bok = [Side]

antLinjer :: Bok -> Int
antLinjer bok = (sum . map length) bok

antOrd :: Bok -> Int
antOrd bok = (sum . map (sum . map length)) bok

eqsLinje :: Linje -> Linje -> Bool
eqsLinje l1 l2 = foldr (\(x, y) acc -> x == y && acc) True (zip (clean l1) (clean l2))

eqsSide :: Side -> Side -> Bool
eqsSide s1 s2 = foldr (\(x, y) acc -> eqsLinje x y && acc) True (zip (clean s1) (clean s2))

eqs :: Bok -> Bok -> Bool
eqs b1 b2 = foldr (\(x, y) acc -> eqsSide x y && acc) True (zip (clean b1) (clean b2))

clean :: [[a]] -> [[a]]
clean [] = []
clean ([] : xs) = clean xs
clean (x : xs) = x : clean xs

b1 = [[["dette", "er", "første linje"], ["pa", "side", "en"]], [], [["siste", "linje"]]]
b2 = [[["dette", "er", "første linje"]], [["pa", "side", "en"], ["siste", "linje"], []]]

e = eqs b1 b2

data Tre t = Nd t [Tre t] deriving (Eq, Show)

noder :: Tre t -> [t]
noder (Nd t []) = [t]
noder (Nd t tre) = [t] ++ foldr (\x acc -> acc ++ noder x) [] tre

t = Nd 1 [Nd 2 [Nd 3 []], Nd 4 [], Nd 5 [Nd 6 [], Nd 7 []]]
