ands :: [Bool] -> Bool
ands [] = True
ands (x : xs) =
  if x == True
    then x == ands xs
    else False
