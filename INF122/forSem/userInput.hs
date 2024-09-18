main = do
  putStrLn "enter a number: "
  num1 <- getLine
  putStrLn "enter a number: "
  num2 <- getLine

  let eval = (read num1 :: Int) + (read num2 :: Int)
  print eval
