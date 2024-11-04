from leapyear import is_leap_year
import random
from datetime import datetime, timedelta

def random_birthday(year):
    year_start = datetime(year, 1, 1)
    days_in_year = 366 if is_leap_year(year) else 365
    year_start + timedelta(days=random.randint(0, days_in_year - 1))
    result = year_start.strftime("%d.%m.%Y")
    return result
    





if __name__ == "__main__":
    print(random_birthday(2022))
    print(random_birthday(2023))
    print(random_birthday(1912))
