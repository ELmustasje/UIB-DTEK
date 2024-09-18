import Test.QuickCheck

digits :: Integer -> [Integer]
digits num = map (\x -> read x) (reverse listNum)
 where
  stringNum = show num
  listNum = map (: []) stringNum
