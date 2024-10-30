{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Main where

import Control.Exception (ArrayException (UndefinedElement), evaluate, handle)
import Data.List
import System.Exit (exitSuccess)
import System.IO
import Test.QuickCheck (Large (getLarge))

{- Message from James:

  Firstly, you do not need to understand this! You can ignore the wird things in this file. But if you want to
  understand it, I have attached an explanation.

  Code grade has a really old version of haskell which does not have readFile'
  I have implemented a version of it such that it will only be defined if your haskell version is as old as code grade's:
    - Line 1 is turning on some language features.
- CPP let's me use the PreProcessor
    - BangPatterns let's me add '!' is clever places to control laziness
    - Line _ is using the preprocessor to say "only keep the following code if ghc's library is older than 4.15.0"
    - line _ uses the bang (!). It makes the function depend on the let expression:
        - length forces str to be fully evaluated
        - let !_ forces my readLine' function to depend on length
      and so together, it forces haskell to read the entire file and close it before we get to return str.
-}

#if ! MIN_VERSION_base(4,15,0)
readFile' :: FilePath -> IO String 
readFile' f = do
  str <- readFile f
  let !_ = length str
  return str
#endif

-- Tip 1: You will definitly want to define a bunch of function to complete this task. Not just implement main.
-- Tip 2: If you want to run your main function from ghci, type :main
main :: IO ()
main = do
  loop

loop :: IO ()
loop = do
  command <- getLine
  execute command
  hFlush stdout

execute :: String -> IO ()
execute command = case words command of
  ("av" : _) -> exitSuccess
  ("hei" : args) -> do
    hei $ unwords args
    loop
  ("vis" : args) -> do
    show_file $ unwords args
    loop
  ("bytt" : old : new : filename) -> do
    bytt old new (unwords filename)
    loop
  _ -> do
    putStrLn "unknown command"
    loop

hei :: String -> IO ()
hei str = putStrLn $ reverse str

show_file :: FilePath -> IO ()
show_file args = do
  x <- readFile args
  putStr x

bytt :: String -> String -> FilePath -> IO ()
bytt old new filename = do
  putStrLn $ "Reading file: " ++ filename
  content <- readFile' filename
  putStrLn "Original content:"
  putStrLn content

  let updatedContent = erstatt_tekst old new content
  putStrLn "Updated content:"
  putStrLn updatedContent

  writeFile filename updatedContent
  putStrLn $ "File '" ++ filename ++ "' has been updated."

-- Replace non-overlapping occurrences of a substring
erstatt_tekst :: String -> String -> String -> String
erstatt_tekst old new = go
 where
  go [] = []
  go str@(x : xs)
    | old `isPrefixOf` str = new ++ go (drop (length old) str)
    | otherwise = x : go xs
