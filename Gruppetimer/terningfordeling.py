from random import randint

fordeling = {1: 0, 2: 0, 3: 0, 4: 0, 5: 0, 6: 0}
antallTrill = 10

for i in range(antallTrill):
    trill = randint(1, 6)
    fordeling[trill] += 1

prosentPoeng = 100 / antallTrill

for key, value in fordeling.items():
    print(key, ": ", "*" * round(prosentPoeng * value))
    print(prosentPoeng * value, "%\n")
