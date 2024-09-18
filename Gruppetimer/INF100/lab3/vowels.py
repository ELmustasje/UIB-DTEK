s = input("Oppgi streng: ")


def fw(s):
    count = 0
    s = s.lower()
    vokaler = ["a", "e", "i", "o", "u"]
    for i in s:
        if i in vokaler:
            count += 1
    if count == 0:
        print("denne strengen inneholder ingen vokal.")
    elif count == 1:
        print(f"Denne strengen inneholder {count} vokal.")
    else:
        print(f"Denne strengen inneholder {count} vokaler.")


fw(s)
