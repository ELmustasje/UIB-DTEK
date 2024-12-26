module Oblig1 where

import Data.Char

tokenise :: String -> [String]
tokenise [] = []
tokenise (x : xs)
  | x == ' ' = tokenise xs
  | x `elem` "*/)(-+" = [x] : tokenise xs
  | isDigit x = takeWhile isDigit (x : xs) : tokenise (dropWhile isDigit xs)
  | isAlpha x = takeWhile isAlpha (x : xs) : tokenise (dropWhile isAlpha xs)
  | otherwise = error "Ulovlig tegn"

data Op = Add | Sub | Mult | Div
  deriving (Show, Eq)
data Ast = BinOp Op Ast Ast | Tall Int | Var String
  deriving (Show, Eq)

isStringNumber :: String -> Bool
isStringNumber = foldr ((&&) . isDigit) True

parseExpr :: [String] -> (Ast, [String])
parseExpr tokens =
  let (termAst, remainingTokens) = parseFactor tokens
   in case remainingTokens of
        ("+" : ts) ->
          let (nextAddend, remainingTokens) = parseExpr ts in (BinOp Add termAst nextAddend, remainingTokens)
        ("-" : ts) ->
          let (nextMinuend, remainingTokens) = parseExpr ts
           in (BinOp Sub termAst nextMinuend, remainingTokens)
        _ -> (termAst, remainingTokens)

parseFactor :: [String] -> (Ast, [String])
parseFactor tokens =
  let (termAst, remainingTokens) = parseTerm tokens
   in case remainingTokens of
        ("*" : ts) ->
          let (nextFactor, restTokens) = parseFactor ts
           in (BinOp Mult termAst nextFactor, restTokens)
 L       ("/" : ts) ->

          let (nextFactor, restTokens) = parseFactor ts
           in (BinOp Div termAst nextFactor, restTokens)
        _ -> (termAst, remainingTokens)

parseTerm :: [String] -> (Ast, [String])
parseTerm ("(" : ts) =
  let (subExpr, remainingTokens) = parseExpr ts
   in case remainingTokens of
        (")" : rest) -> (subExpr, rest)
        _ -> error "Forventet lukket parantes"
parseTerm (x : xs)
  | isStringNumber x = (Tall (read x), xs)
  | all isAlpha x = (Var x, xs)
parseTerm _ = error "Forventet et tall eller en variabel"

parse :: String -> Ast
parse str = fst $ parseExpr $ tokenise str

eval :: Ast -> Int
eval (Tall n) = n
eval (BinOp op left right) =
  let lval = eval left
      rval = eval right
   in case op of
        Add -> lval + rval
        Sub -> lval - rval
        Mult -> lval * rval
        Div -> lval `div` rval

ppInfix :: Ast -> String
ppInfix (Tall n) = show n
ppInfix (Var v) = v
ppInfix (BinOp op left right) =
  let lStr = ppInfix left
      rStr = ppInfix right
      opStr = case op of
        Add -> "+"
        Sub -> "-"
        Mult -> "*"
        Div -> "/"
   in "(" ++ lStr ++ " " ++ opStr ++ " " ++ rStr ++ ")"

ppPN :: Ast -> String
ppPN (Tall n) = show n
ppPN (Var v) = v
ppPN (BinOp op left right) =
  let lstr = ppPN left
      rstr = ppPN right
      opStr = case op of
        Add -> "+"
        Sub -> "-"
        Mult -> "*"
        Div -> "/"
   in opStr ++ " " ++ lstr ++ " " ++ rstr

ppOPN :: Ast -> String
ppOPN (Tall n) = show n
ppOPN (Var v) = v
ppOPN (BinOp op left right) =
  let lstr = ppOPN left
      rstr = ppOPN right
      opStr = case op of
        Add -> "+"
        Sub -> "-"
        Mult -> "*"
        Div -> "/"
   in lstr ++ " " ++ rstr ++ " " ++ opStr

findVar :: [(String, Int)] -> String -> Int
findVar [] _ = 0
findVar (x : xs) v
  | fst x == v = snd x
  | otherwise = findVar xs v

evalVar :: Ast -> [(String, Int)] -> Int
evalVar (Tall n) _ = n
evalVar (Var v) ls = findVar ls v
evalVar (BinOp op left right) ls =
  let lval = evalVar left ls
      rval = evalVar right ls
   in case op of
        Add -> lval + rval
        Sub -> lval - rval
        Mult -> lval * rval
        Div -> lval `div` rval
