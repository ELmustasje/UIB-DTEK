ord_1 = input("skriv et ord: ")
ord_2 = input("Skriv et annet ord: ")
ord_3 = input("Skriv et siste ord: ")

ord = [ord_1, ord_2, ord_3]
shortest = []

for i in ord:
    if not shortest:
        shortest.append(i)
    elif len(i) == len(shortest[0]):
        shortest.append(i)
    elif len(i) < len(shortest[0]):
        shortest = []
        shortest.append(i)

for i in shortest:
    print(i)
