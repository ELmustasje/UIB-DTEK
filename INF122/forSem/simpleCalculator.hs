getNum :: IO Double
getNum = do
  putStrLn "Enter a Number"
  num <- getLine
  return (read num :: Double)

getOperator :: IO String
getOperator = do
  putStrLn "Enter the opetator (+,-,*,/)"
  operator <- getLine
  case operator of
    "+" -> return operator
    "-" -> return operator
    "/" -> return operator
    "*" -> return operator
    _ -> do
      putStrLn "Not valid operator"
      getOperator

calc :: Double -> Double -> String -> IO Double
calc num1 num2 operator
  | operator == "+" = return (num1 + num2)
  | operator == "-" = return (num1 - num2)
  | operator == "/" = return (num1 / num2)
  | operator == "*" = return (num1 * num2)
  | otherwise = do
      putStrLn "error"
      return 0

main :: IO ()
main = do
  num1 <- getNum
  operator <- getOperator
  num2 <- getNum
  res <- calc num1 num2 operator
  print res
