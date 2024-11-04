def is_leap_year(year):
    # TODO: skriv koden din for del A her
    # (har du allereie gjorde oppgåva i lab 2, kan du kopiere derfrå)
    ...
    if (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0):
        return True
    else:
        return False
                

def main():
    # TODO: skriv koden din for del B her
    ...
    year = int(input('Skriv inn eit årstal:\n'))
    if is_leap_year(year):
        print(f'{year} er eit skotår')
    else:
        print(f'{year} er ikkje eit skotår')
        
if __name__ == "__main__":
    # Vi kallar main berre viss *denne* fila blir køyrd som hovudprogrammet
    main()
