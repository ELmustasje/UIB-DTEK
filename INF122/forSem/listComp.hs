simpleList = [x ^ 2 | x <- [1 .. 10]]

onlyOdd = [x | x <- simpleList, odd x]

tupples = [("thomas", "barth", 2004, True), ("markus", "barth", 2000, True), ("lucas", "bish", 2022, True), ("aina", "mogstad", 1973, False)]

brothers = [(first, last, year) | (first, last, year, isBrother) <- tupples, isBrother]
