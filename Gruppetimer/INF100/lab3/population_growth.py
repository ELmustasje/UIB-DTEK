start = int(input("Befolkning: "))
vekst = float(input("Årlig vekstrate (i prosent): "))
tid = int(input("Antall år: "))

for i in range(1, tid + 1):
    print(f"Befolkningen etter {i} år er", round(start * (1 + vekst / 100) ** i))
totpr = round((((start * (1 + vekst / 100) ** tid) - start) / start) * 100, 2)
print(f"Total vekst etter {tid} år er (i prosent) {totpr}")


print(
    """


      """
)
