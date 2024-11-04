def is_leap_year(year):
    if (year % 4 == 0) and (year % 100 == 0) and (year % 400 == 0):
        return True
    elif (year % 4 == 0) and (year % 100 != 0):
        return True
    else:
        return False

def main():
    year_from_user = int(input('Skriv inn eit årstal:\n'))
    is_or_not = is_leap_year(year_from_user)
    if is_or_not == True:
        print(f'{year_from_user} er eit skotår')
    else:
        print(f'{year_from_user} er ikkje eit skotår')
          
if __name__ == "__main__":
    # Vi kallar main berre viss *denne* fila blir køyrd som hovudprogrammet
    main()