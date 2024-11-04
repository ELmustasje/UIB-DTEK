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



def main(year):
    print('Skriv inn eit årstal:')
    year = int(input())
    skotår = is_leap_year(year)
    if skotår == True:
        print(year, 'er eit skotår')
    elif skotår == False:
        print(year, 'er ikkje eit skotår')

if __name__=='__main__':
    main('year')