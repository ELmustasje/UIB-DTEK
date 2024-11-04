
def is_leap_year(year):
    if year % 4 == 0:
        if year % 100 == 0:
            if year % 400 == 0:
                return True
            else:
                return False
        else:
            return True
    else:
        return False
    
def main():
    year = int(input("Skriv inn eit årstal:\n"))
    if is_leap_year(year) == True:
        print(f"{year} er eit skotår")
    else:
        print(f"{year} er ikkje eit skotår")

if __name__ == "__main__":
    main()