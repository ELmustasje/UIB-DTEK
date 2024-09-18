module Week37Exercise0 where

information name department year = map createInformationSentence approvedEnteries
 where
  zippedEnteries = zip3 name department year
  approvedEnteries = [(name, department, year) | (name, department, year) <- zippedEnteries, year >= 2022]
  createInformationSentence (name, department, year) = name ++ " is studying at " ++ department ++ " department and started in " ++ show year
