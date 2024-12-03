module Oblig1 where

import Data.Char
import Data.Void (vacuous)
import Distribution.Simple.Utils (xargs)
import System.OsPath.Posix (isAbsolute)

tokenise :: String -> [String]
tokenise [] = []
tokenise (x : xs)
  | x == ' ' = tokenise xs
  | x `elem` "*/)(-+" = [x] : tokenise xs
  | isDigit x = takeWhile isDigit (x : xs) : tokenise (dropWhile isDigit xs)
  | isAlpha x = takeWhile isAlpha (x : xs) : tokenise (dropWhile isAlpha xs)
  | otherwise = error "bad char"

data Op = Add | Sub | Mult | Div
  deriving (Show, Eq)

data Ast = BinOp Op Ast Ast | Tall Int
  deriving (Show, Eq)

parseExpr :: [String] -> (Ast, [String])
parseExpr xs =
  let (parsed, rem) = parseFactor xs
   in case rem of
        ("+" : xs) -> (BinOp Add parsed nextExpr, restTokens)
         where
          (nextExpr, restTokens) = parseFactor xs
        ("-" : xs) -> (BinOp Sub parsed nextExpr, restTokens)
         where
          (nextExpr, restTokens) = parseFactor xs
        _ -> (parsed, rem)

parseFactor :: [String] -> (Ast, [String])
parseFactor xs =
  let (parsed, rem) = parseTerm xs
   in case rem of
        ("*" : xs) -> (BinOp Mult parsed nextFactor, restTokens)
         where
          (nextFactor, restTokens) = parseTerm xs
        ("/" : xs) -> (BinOp Div parsed nextFactor, restTokens)
         where
          (nextFactor, restTokens) = parseTerm xs
        _ -> (parsed, rem)

parseTerm :: [String] -> (Ast, [String])
parseTerm ("(" : xs) =
  let (parsed, rem) = parseExpr xs
   in case rem of
        (")" : rest') -> (parsed, rest')
        _ -> error "no closing parent"
parseTerm (x : xs)
  | isDigit (head x) = (Tall (read x), xs)
  | otherwise = parseExpr (x : xs)

parse :: String -> Ast
parse str = fst $ parseExpr $ tokenise str

eval :: Ast -> Int
eval (Tall x) = x
eval (BinOp op left right) =
  let leftInt = eval left
      rigthInt = eval right
   in case op of
        Add -> leftInt + rigthInt
        Sub -> leftInt - rigthInt
        Mult -> leftInt * rigthInt
        Div -> leftInt `div` rigthInt

ppInfix :: Ast -> String
ppInfix = undefined

ppPN :: Ast -> String
ppPN = undefined

ppOPN :: Ast -> String
ppOPN = undefined

findVar :: [(String, Int)] -> String -> Int
findVar xs _ = 0
findVar (x : xs) v
  | fst x == v = snd x
  | otherwise = findVar xs v

evalVar :: Ast -> [(String, Int)] -> Int
evalVar = undefined
