import random
from datetime import datetime, timedelta

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

def random_birthday(year):
    #   Jeg vil strukturere en dict med måneder som nøkler, og antall dager som verdi.
    
    january_first = datetime(year, 1, 1)

    if is_leap_year(year) == True:
        year_length = 366
    else:
        year_length = 365
    
    #   Liste over antall dager som kan legges til

    day_list = []
    for i in range(year_length - 1):
        day_list.append(i)
    
    #   Tilfeldig mengde dager legges til

    num_days = random.choice(day_list)

    random_day = january_first + timedelta(days = num_days)
    random_day = random_day.strftime("%d.%m.%Y")

    return random_day

if __name__ == "__main__":
    print(random_birthday(2022))
    print(random_birthday(2023))
    print(random_birthday(1912))
