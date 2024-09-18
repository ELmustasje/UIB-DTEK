x1 = float(input("x_lo = "))
x2 = float(input("x_hi = "))
ant = float(input("n = "))


def biter(x1, x2, ant):
    dist = abs(x1 - x2)
    delta = dist / ant
    punkt = x1
    while punkt != x2:
        punkt += delta
        print(punkt - delta, punkt)


biter(x1, x2, ant)
