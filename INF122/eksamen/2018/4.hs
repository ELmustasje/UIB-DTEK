import Data.Char
import System.Exit

tokenise :: String -> [String]
tokenise (x : xs)
  | x == ' ' = tokenise xs
  | x `elem` "+*" = [x] : tokenise xs
  | isDigit x = takeWhile isDigit (x : xs) : tokenise (dropWhile isDigit (x : xs))

parseAndEval :: [String] -> (Int, [String])
parseAndEval (x : xs)
  | isDigit $ head x = (read x, xs)
  | x == "+" =
      let (first, rest) = parseAndEval xs
          (second, rest') = parseAndEval rest
       in (first + second, rest')
  | x == "*" =
      let (first, rest) = parseAndEval xs
          (second, rest') = parseAndEval rest
       in (first * second, rest')

ev :: String -> Int
ev str = fst $ parseAndEval $ tokenise str

fun :: IO ()
fun = do
  input <- getLine
  execute input 

execute input
  | input == "" = exitSuccess 
  | otherwise = do 
    print $ ev input 
    fun

