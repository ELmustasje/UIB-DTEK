from decimal import Decimal

s1 = Decimal(input("sideA = "))
s2 = Decimal(input("sideB = "))
s3 = Decimal(input("sideC = "))


def er_trekant(s1, s2, s3):
    if (s1 + s2) > s3 and (s1 + s3) > s2 and (s2 + s3) > s1:
        print("Mulig trekant.")
    elif (s1 + s2) == s3 or (s1 + s3) == s2 or (s2 + s3) == s1:
        print("Noen vil si det er en trekant, andre vil si det bare er en strek.")
    else:
        print("Umulig å lage en slik trekant.")


er_trekant(s1, s2, s3)
