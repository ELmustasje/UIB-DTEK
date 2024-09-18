from math import dist

a = [int(input("x1 = ")), int(input("y1 = "))]
b = [int(input("x2 = ")), int(input("y2 = "))]

print(f"avstanden mellom ({a[0]},{a[1]}) og ({b[0]},{b[1]}) er ", dist(a, b))
