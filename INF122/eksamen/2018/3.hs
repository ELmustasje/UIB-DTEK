import Data.Fixed (E0)

data Tre = Blad Int | Node Tre Tre
a = Blad 0
b = Node (Blad 0) (Blad 0)
c = Node (Node (Node (Blad 0) (Blad 0)) (Blad 0)) (Node (Blad 0) (Blad 0))

antB :: Tre -> Int
antB (Blad n) = 1
antB (Node left right) = antB left + antB right

dybde :: Tre -> Int
dybde tre = aux tre 0
 where
  aux (Blad n) d = d
  aux (Node l r) d = max (aux l (d + 1)) (aux r (d + 1))

komp :: Tre -> Bool
komp (Blad n) = False
komp (Node (Blad n) (Blad g)) = True
komp (Node l r) = if antB l == antB r then komp l == komp r else False

lagK :: Int -> Tre
lagK 0 = Blad 0
lagK n = Node (lagK (n - 1)) (lagK (n - 1))

d = dybde $ lagK 4
