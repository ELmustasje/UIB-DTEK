import System.IO

main = do
  content <- readFile' "input.txt"
  let count = countXmas $ lines content
  print count

isXmas :: Int -> Int -> [String] -> Bool
isXmas x y arr = (first == "MAS" || reverse first == "MAS") && (second == "MAS" || reverse second == "MAS")
 where
  first = [(arr !! (y - 1) !! (x - 1))] ++ [(arr !! (y) !! (x))] ++ [(arr !! (y + 1) !! (x + 1))]
  second = [(arr !! (y + 1) !! (x - 1))] ++ [(arr !! (y) !! (x))] ++ [(arr !! (y - 1) !! (x + 1))]

countXmas arr = foldr (\x acc -> if x == True then acc + 1 else acc) 0 [isXmas x y arr | x <- [1 .. length (head arr) - 2], y <- [1 .. length arr - 2]]

findHorisontalAndVertical :: String -> Int
findHorisontalAndVertical str =
  foldr (\x acc -> acc + (countLine x)) 0 (rowStr)
    + foldr (\x acc -> acc + (countLine x)) 0 (rStr)
    + foldr (\x acc -> acc + (countLine x)) 0 (fStr)
    + foldr (\x acc -> acc + (countLine x)) 0 (fRStr)
    + foldr (\x acc -> acc + (countLine x)) 0 (f90Str)
    + foldr (\x acc -> acc + (countLine x)) 0 (fR90Str)
    + foldr (\x acc -> acc + (countLine x)) 0 (f90RStr)
    + foldr (\x acc -> acc + (countLine x)) 0 (f90RRStr)
 where
  rowStr = lines str
  rStr = map reverse rowStr
  fStr = flip2DArr $ lines str
  fRStr = map reverse fStr
  f90Str = (topHalfDiagnol $ lines str) ++ (bottomHalfDiagnol $ lines str)
  fR90Str = map reverse f90Str
  f90RStr = (topHalfDiagnol $ reverse $ lines str) ++ (bottomHalfDiagnol $ reverse $ lines str)
  f90RRStr = map reverse f90RStr

flip2DArr :: [[a]] -> [[a]]
flip2DArr arr = foldr (\x acc -> (getColumnN x arr) : acc) [] [0 .. (length $ head arr) - 1]

topHalfDiagnol :: [[a]] -> [[a]]
topHalfDiagnol arr = foldr (\x acc -> (getDiagnolN x arr) : acc) [] [0 .. (length $ head arr) - 1]

bottomHalfDiagnol :: [[a]] -> [[a]]
bottomHalfDiagnol arr = aux (tail arr) []
 where
  aux [] acc = acc
  aux (x : xs) acc = aux xs (acc ++ [getDiagnolN 0 (x : xs)])

getColumnN :: Int -> [[a]] -> [a]
getColumnN n arr = foldr (\x acc -> (arr !! x) !! n : acc) [] [0 .. length arr - 1]

getDiagnolN :: Int -> [[a]] -> [a]
getDiagnolN n arr = aux arr n []
 where
  aux [] _ acc = acc
  aux (x : xs) n acc = if n >= length x then acc else aux xs (n + 1) (acc ++ [x !! n])

countLine :: String -> Int
countLine line = acc line 0
 where
  acc [] n = n
  acc (x : xs) n
    | take 4 (x : xs) == "XMAS" = acc (xs) n + 1
    | otherwise = acc xs n
