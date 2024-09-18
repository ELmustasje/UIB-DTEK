tall = int(input("Oppgi et positivt heltall: "))
maxlen = len(str(tall**2))


for i in range(1, tall + 1):
    print(
        " " * (len(str(tall)) - len(str(i))),
        f"{i}:",
        " " * (len(str(tall)) - len(str(i))),
        end="",
    )
    for g in range(1, tall + 1):
        print(str(i * g) + (" " * (maxlen - len(str((g + 1) * i)))), end=" ")
    print()
