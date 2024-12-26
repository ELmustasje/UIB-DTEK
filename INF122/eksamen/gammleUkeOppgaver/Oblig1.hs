import Control.Arrow (ArrowChoice (right))
import Data.Char

tokenise :: String -> [String]
tokenise [] = []
tokenise (x : xs)
  | x == ' ' = tokenise xs
  | x `elem` "+-*/)(" = [x] : tokenise xs
  | isDigit x = takeWhile isDigit (x : xs) : tokenise (dropWhile isDigit (x : xs))
  | isAlpha x = takeWhile isAlpha (x : xs) : tokenise (dropWhile isAlpha (x : xs))
  | otherwise = error "not valid symbol"

data Op = Add | Sub | Mult | Div deriving (Eq, Show)
data Ast = BinOp Op Ast Ast | Tall Int deriving (Eq, Show)

parseTerm :: [String] -> (Ast, [String])
parseTerm (x : xs)
  | isDigit (head x) = (Tall (read x), xs)
  | x == "(" =
      let (exp, rest) = parseExpr xs
       in case rest of
            (")" : rest'') -> (exp, rest'')
            _ -> error "need closing parenthesis"

parseFactor xs =
  let (term, rest) = parseTerm xs
   in case rest of
        ("*" : rest') -> let (factor, rest'') = parseFactor rest' in (BinOp Mult term factor, rest'')
        ("/" : rest') -> let (factor, rest'') = parseFactor rest' in (BinOp Div term factor, rest'')
        _ -> (term, rest)

parseExpr xs =
  let (factor, rest) = parseFactor xs
   in case rest of
        ("+" : rest') -> let (exp, rest'') = parseExpr rest' in (BinOp Add factor exp, rest'')
        ("-" : rest') -> let (exp, rest'') = parseExpr rest' in (BinOp Sub factor exp, rest'')
        _ -> (factor, rest)

parse :: String -> Ast
parse str = fst $ parseExpr $ tokenise str

eval :: Ast -> Int
eval (Tall n) = n
eval (BinOp op right left) =
  case op of
    Add -> eval right + eval left
    Sub -> eval right - eval left
    Div -> eval right `div` eval left
    Mult -> eval right * eval left
