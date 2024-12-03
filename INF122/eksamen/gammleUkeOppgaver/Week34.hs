isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

safeHead :: [a] -> [a]
safeHead [] = []
safeHead xs = [head xs]
