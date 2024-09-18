import Control.Arrow (Arrow (first))

scores :: [Int]
scores = [79, 83, 100]

main :: IO ()
main = do
  print (scores !! 1) -- grabs the index 1
  print (head scores) -- grabs the first
