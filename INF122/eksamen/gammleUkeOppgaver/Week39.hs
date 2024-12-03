import Language.Haskell.TH (Body)

data Set a = Empty | Singleton a | Union (Set a) (Set a)
  deriving (Eq, Show)

fromList :: [a] -> Set a
fromList [] = Empty
fromList [x] = Singleton x
fromList (x : xs) = Union (Singleton x) (fromList xs)

isIn :: (Eq a) => a -> Set a -> Bool
isIn el (Singleton a) = el == a
isIn el Empty = False
isIn el (Union left rigth) = isIn el left || isIn el rigth

subSet :: (Eq a) => Set a -> Set a -> Bool
subSet (Singleton a) b = isIn a b
subSet (Union left rigth) b = subSet left b && subSet rigth b

setEq :: (Eq a) => Set a -> Set a -> Bool
setEq a b = subSet a b && subSet b a

data Nat = Zero | Succ Nat
  deriving (Show, Eq)

foldNat :: a -> (a -> a) -> Nat -> a
foldNat s f Zero = s
foldNat s f (Succ r) = foldNat (f s) f r

foldSet :: b -> (a -> b) -> (b -> b -> b) -> Set a -> b
foldSet s op f Empty = s
foldSet s op f (Singleton a) = foldSet (f (op a) s) op f Empty
foldSet s op f (Union left right) = foldSet (foldSet s op f left) op f right

isInF :: (Eq a) => a -> Set a -> Bool
isInF x s = foldSet False (== x) (||) s

subSetF :: (Eq a) => Set a -> Set a -> Bool
subSetF x s = foldSet True (\x -> isInF x s) (&&) s
