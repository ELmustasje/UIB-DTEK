def is_leap_year(year):
    if year % 400 == 0:
        return True
    elif year % 100 == 0:
        return False
    elif year % 4 == 0:
        return True 
    else:
        return False


def main():
    årstall = int(input('Skriv inn eit årstal:\n'))
    is_leap_year(årstall)
    if is_leap_year(årstall) == True:
        print (f'{årstall} er eit skotår')
    else:
        print(f'{årstall} er ikkje eit skotår')

if __name__ == "__main__":
    main()

    