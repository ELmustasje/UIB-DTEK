ord_1 = input("Skriv et ord: ")
ord_2 = input("Skriv et annet ord: ")
ord_3 = input("Skriv et siste ord: ")

print(max(ord_1, ord_2, ord_3, key=len))
