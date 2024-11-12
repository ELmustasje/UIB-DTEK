{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Oblig2 where

import Data.Char
import Data.List
import Debug.Trace (trace)
import GHC.IO.Handle.Types (HandleType (ReadWriteHandle))
import System.IO
import Test.QuickCheck

#if ! MIN_VERSION_base(4,15,0)
readFile' :: FilePath -> IO String
readFile' f = do
  str <- readFile f
  let !_ = length str
  return str
#endif

data Regex = Atom Char | Both Regex Regex | After Regex Regex | Kleene Regex | Empty | Any
  deriving (Show, Eq)

reg [] = (Empty, "")
reg input =
  let (r, rest) = re1 input
   in case rest of
        [] -> (r, rest)
        (')' : rest') -> (r, rest')
        _ -> let (r', rest') = reg rest in (After r r', rest')
re1 input =
  let (r, rest) = re2 input
   in case rest of
        ('*' : rest') -> (Kleene r, rest')
        _ -> (r, rest)
re2 input =
  let (r, rest) = re3 input
   in case rest of
        ('|' : rest') ->
          let (r', rest'') = re2 rest'
           in (Both r r', rest'')
        _ -> (r, rest)
re3 (c : cs)
  | c == '.' = (Any, cs)
  | c == '(' = reg cs
  | otherwise = (Atom c, cs)

type Transition = String -> [(String, String)]
matchChar :: Char -> Transition
matchChar c (x : xs)
  | c == x = [(c : "", xs)]
  | otherwise = []
matchChar _ [] = []

matchAny :: Transition
matchAny (x : xs) = [(x : "", xs)]
matchAny [] = []

matchEmpty :: Transition
matchEmpty x = [("", x)]

matchBoth :: Transition -> Transition -> Transition
matchBoth a b input = a input ++ b input

matchAfter :: Transition -> Transition -> Transition
matchAfter trans1 trans2 s = [(match1 ++ match2, rest2) | (match1, rest1) <- trans1 s, (match2, rest2) <- trans2 rest1]

regex2trans :: Regex -> Transition
regex2trans Empty = matchEmpty
regex2trans (Atom c) = matchChar c
regex2trans Any = matchAny
regex2trans (Both r1 r2) = matchBoth (regex2trans r1) (regex2trans r2)
regex2trans (After r1 r2) = matchAfter (regex2trans r1) (regex2trans r2)
regex2trans (Kleene r) = matchKleene (regex2trans r)

matchKleene :: Transition -> Transition
matchKleene trans s =
  [("", s)]
    ++ [ (match1 ++ match1s, rest1s)
       | (match1, rest1) <- trans s
       , (match1s, rest1s) <- matchKleene trans rest1
       ]

longest :: [String] -> String
longest = foldl (\acc x -> if length x > length acc then x else acc) ""

matchStart :: String -> String -> String
matchStart pattern input =
  longest [match | (match, rest) <- transInput, not (null match)]
 where
  regex = fst $ reg pattern
  transInput = regex2trans regex input

matchLine :: String -> String -> String
matchLine pattern line =
  longest (map (matchStart pattern) (tails line))

replaceLine :: String -> (String -> String) -> String -> String
replaceLine pattern f text = go text
 where
  regex = fst $ reg pattern
  go "" = ""
  go t = case match t of
    [] -> let (c : cs) = t in c : go cs -- Hvis ingen match, ta første karakter og fortsett rekursjon
    ((match, rest) : _) -> f match ++ go rest -- Hvis match, erstatt og fortsett rekursjon
  match = regex2trans regex

grep :: String -> FilePath -> IO ()
grep pattern filePath = do
  content <- readFile filePath
  let matchingLines = filter (not . null . matchLine pattern) (lines content)
  mapM_ putStrLn matchingLines

sed :: String -> (String -> String) -> FilePath -> FilePath -> IO ()
sed pattern f inputFilePath outputFilePath = do
  -- Åpne inputfilen for lesing

  content <- readFile' inputFilePath
  let replacedContent = unlines (map (replaceLine pattern f) (lines content))
  writeFile outputFilePath replacedContent
