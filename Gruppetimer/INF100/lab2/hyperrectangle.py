x1 = input("x1 = ")
y1 = input("y1 = ")
x2 = input("x2 = ")
y2 = input("y2 = ")
xp = input("xp = ")
yp = input("yp = ")


venstre = min(x1, x2)
høyre = max(x1, x2)
oppe = max(y1, y2)
nede = min(y1, y2)

if xp >= venstre and xp <= høyre and yp >= nede and yp <= oppe:
    print("inne")
else:
    print("ute")
