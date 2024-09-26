module Week39 where

data Set a = Empty | Singleton a | Union (Set a) (Set a)
  deriving (Eq, Show)

fromList :: [a] -> Set a
fromList (x : xs)
  | elem x xs = fromList xs
  | otherwise = [x] ++ fromList xs

isIn :: (Eq a) => a -> Set a -> Bool
isIn = undefined

subset :: (Eq a) => Set a -> Set a -> Bool
subset = undefined

setEq :: (Eq a) => Set a -> Set a -> Bool
setEq = undefined

data Nat = Zero | Succ Nat
  deriving (Show, Eq)

foldNat :: a -> (a -> a) -> Nat -> a
foldNat = undefined

foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet = undefined

isInF :: (Eq a) => a -> Set a -> Bool
isInF = undefined

subsetF :: (Eq a) => Set a -> Set a -> Bool
subsetF = undefined
