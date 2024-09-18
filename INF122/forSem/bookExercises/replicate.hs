replicates :: Int -> a -> [a]
replicates 0 a = []
replicates n a = [a] ++ replicates (n - 1) a
