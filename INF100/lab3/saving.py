def main():
    årslønn = float(input("Hva er årslønn?: "))
    prosent = float(input("Hvor mange prosent spares?: "))
    totkost = float(input("Hvor mye koster boligen?: "))
    totkost_andel = totkost * 0.25

    måndtlig_sparing = (årslønn / 12) * (prosent / 100)
    spart = 0

    rente = 0.04

    måneder = 0
    while spart < totkost_andel:
        rentesparing = spart * (rente / 12)
        spart += rentesparing + måndtlig_sparing
        måneder += 1
    print(f"Det tar {måneder} måneder å spare nok egenkapital.")


if __name__ == "__main__":
    main()
