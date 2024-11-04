

import random
from datetime import date, timedelta, datetime

def is_leap_year(year):
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)

def random_birthday(year):
    y = datetime(year, 1, 1).replace(day=1, month=1).toordinal()
    end_dt = datetime(year, 12, 31).toordinal()
    random_day = date.fromordinal(random.randint(y, end_dt))
    random_day = random_day.strftime('%d.%m.%Y')
    return random_day
    if is_leap_year(year):
        return (f'{random_day} er et skuddår')
    else:
        return (f'{random_day} er ikke et skuddår')
     



""" if __name__ == "__main__":
    print(random_birthday(2022))
    print(random_birthday(2023))
    print(random_birthday(1912))
 """