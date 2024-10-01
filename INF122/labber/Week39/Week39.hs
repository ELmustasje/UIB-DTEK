module Week39 where

import Control.Arrow (ArrowChoice (right))
import Data.List

data Set a = Empty | Singleton a | Union (Set a) (Set a)
  deriving (Eq, Show)

fromList :: [a] -> Set a
fromList [] = Empty
fromList (x : xs) = Union (Singleton x) (fromList xs)

isIn :: (Eq a) => a -> Set a -> Bool
isIn a Empty = False
isIn a (Singleton b) = a == b
isIn a (Union left right) = isIn a left || isIn a right

subset :: (Eq a) => Set a -> Set a -> Bool
subset Empty _ = True
subset (Singleton a) b = isIn a b
subset (Union left right) b = subset left b && subset right b

setEq :: (Eq a) => Set a -> Set a -> Bool
setEq s1 s2 = subsetF s1 s2 && subsetF s2 s1

data Nat = Zero | Succ Nat
  deriving (Show, Eq)

foldNat :: a -> (a -> a) -> Nat -> a
foldNat z _ Zero = z
foldNat z f (Succ n) = f (foldNat z f n)

foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet e f g = go
 where
  go Empty = e
  go (Singleton x) = f x
  go (Union l r) = g (go l) (go r)

isInF :: (Eq a) => a -> Set a -> Bool
isInF x = foldSet False (== x) (||)

subsetF :: (Eq a) => Set a -> Set a -> Bool
subsetF s1 s2 = foldSet True (\x -> isInF x s2) (&&) s1
