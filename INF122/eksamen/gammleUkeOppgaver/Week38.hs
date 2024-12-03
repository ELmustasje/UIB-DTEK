remg :: [a] -> (a -> Bool) -> [a]
remg (x : xs) f
  | f x = xs
  | otherwise = x : remg xs f

mapF f xs = foldr (\x acc -> (f x) : acc) [] xs 
