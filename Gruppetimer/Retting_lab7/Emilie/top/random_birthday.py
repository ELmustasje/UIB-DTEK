# kode hentet fra løsningsforslag, INF100 siden
from datetime import datetime, timedelta
from leapyear import is_leap_year
import random
def random_birthday (year):
    day = datetime(year=year, month=1, day=1)
    days_in_year = 366 if is_leap_year (year) else 365
    day += timedelta(days=random.choice(range(days_in_year)))
    result = day.strftime ('%d.%m.%Y')
    return result 


if __name__ == "__main__":
    print(random_birthday(2022))
    print(random_birthday(2023))
    print(random_birthday(1912))
