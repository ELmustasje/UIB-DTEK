def is_leap_year(year):
    if (year % 400) == 0:
        return True
    elif (year % 100) == 0:
        return False
    elif (year % 4) == 0:
        return True
    else:
        return False
    
def main():
    print("Skriv inn eit årstal:")  
    try: 
        f = int(input())
        if is_leap_year(f) == True:
            print(f'{f} er eit skotår')
        else:
            print(f'{f} er ikkje eit skotår')
    except:
        print('Skriv inn et tall')


main()

