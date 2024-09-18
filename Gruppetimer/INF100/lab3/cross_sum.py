ant = int(input("n = "))
tverrsum = int(input("x = "))


def sumSiffer(tall):
    tall = str(tall)
    a = sum(int(siffer) for siffer in tall)
    return a


funnet = []
i = 0
while len(funnet) != ant:
    if sumSiffer(i) == tverrsum:
        funnet.append(i)
    i += 1

print(f"Det {ant}. tallet med tverrsum {tverrsum} er {max(funnet)}.")
