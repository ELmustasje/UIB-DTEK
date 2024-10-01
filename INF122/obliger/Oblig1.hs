module Oblig1 where

import Data.Char

tokenise :: String -> [String]
tokenise [] = []

data Op = Add | Sub | Mult | Div
  deriving (Show, Eq)

data Ast = BinOp Op Ast Ast | Tall Int
  deriving (Show, Eq)

parseExpr :: [String] -> (Ast, [String])
parseExpr = undefined

parseFactor :: [String] -> (Ast, [String])
parseFactor = undefined

parseTerm :: [String] -> (Ast, [String])
parseTerm = undefined

parse :: String -> Ast
parse = undefined

eval :: Ast -> Int
eval = undefined

ppInfix :: Ast -> String
ppInfix = undefined

ppPN :: Ast -> String
ppPN = undefined

ppOPN :: Ast -> String
ppOPN = undefined

findVar :: [(String, Int)] -> String -> Int
findVar = undefined

evalVar :: Ast -> [(String, Int)] -> Int
evalVar = undefined
