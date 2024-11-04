# Kode hentet fra løsningsforalsag video INF100 nettsiden, lab7

#Del A
def is_leap_year (year):
    if year % 400 == 0:
        return True 
    if year % 100 == 0:
        return False 
    return year % 4 == 0

# Del B
def main ():
    print ('Skriv inn eit årstal:')
    year=int(input())
    if is_leap_year(year):
        print(f'{year} er eit skotår')
    else:
        print(f'{year} er ikkje eit skotår')



if __name__ == '__main__':
    main()