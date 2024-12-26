import Data.Char (isDigit)

data BinTree a = Empty | Leaf Int | Bransh (BinTree Int) Int (BinTree Int)

main = do
  let a = Bransh (Leaf 2) 10 (Leaf 3)
  let b = foldt (\x acc -> acc + x) 0 a
  print b

foldt f acc Empty = acc
foldt f acc (Leaf a) = f a acc
foldt f acc (Bransh l a r) = foldt f (f a (foldt f acc r)) l

printTree :: (Show a) => BinTree a -> IO ()
printTree (Leaf a) = print a

tokenise :: String -> [String]
tokenise [] = []
tokenise (x : xs)
  | x `elem` "+-*)(/" = [x] : tokenise xs
  | isDigit x = takeWhile isDigit (x : xs) : (tokenise $ dropWhile isDigit (x : xs))
  | otherwise = tokenise xs

tailRev :: [a] -> [a]
tailRev xs = aux xs []
 where
  aux [] acc = acc
  aux (x : xs) acc = aux xs (x : acc)

fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
fibSq n = [fib x | x <- [0 .. n]]
