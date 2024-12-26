data Tre = Blad Int | Node Tre Tre

erBlad (Blad a) = a
erBlad (Node l r) = erBlad l + erBlad r

a = Blad 0
b = Node (Blad 0) (Blad 0)
c = Node (Node (Blad 0) (Blad 0)) (Blad 0)

antB (Blad a) = 1
antB (Node left right) = antB left + antB right

dybde (Blad a) = 0
dybde (Node l r) = 1 + max (dybde l) (dybde r)

komp (Blad a) = False
komp (Node (Blad a) (Blad b)) = True
komp (Node left right) = antB left == antB right && komp left && komp right

lagK 1 = Node (Blad 0) (Blad 0)
lagK n = Node (lagK (n - 1)) (lagK (n - 1))
