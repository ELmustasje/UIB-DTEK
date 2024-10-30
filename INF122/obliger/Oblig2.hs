{-# LANGUAGE BangPatterns, CPP #-}
module Oblig2 where

import Data.Char
import Data.List
import System.IO

#if ! MIN_VERSION_base(4,15,0)
readFile' :: FilePath -> IO String
readFile' f = do
  str <- readFile f
  let !_ = length str
  return str
#endif


data Regex = Atom Char | Both Regex Regex | After Regex Regex | Kleene Regex | Empty | Any
  deriving (Show, Eq)

reg, re1, re2, re3 :: String -> (Regex, String)

reg = undefined
re1 = undefined
re2 = undefined
re3 = undefined

type Transition = String -> [(String,String)]

matchChar :: Char -> Transition
matchChar = undefined

matchAny :: Transition
matchAny = undefined

matchEmpty :: Transition
matchEmpty = undefined

matchBoth :: Transition -> Transition -> Transition
matchBoth = undefined

matchAfter :: Transition -> Transition -> Transition
matchAfter = undefined

regex2trans :: Regex -> Transition
regex2trans = undefined

longest :: [String] -> String
longest = undefined

matchStart :: String -> String -> String
matchStart = undefined

matchLine :: String -> String -> String
matchLine = undefined

replaceLine :: String -> (String -> String) -> String -> String
replaceLine = undefined

grep :: String -> FilePath -> IO ()
grep = undefined

sed :: String -> (String -> String) -> FilePath -> FilePath -> IO ()
sed = undefined
