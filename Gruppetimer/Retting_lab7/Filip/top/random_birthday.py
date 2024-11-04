import random
from datetime import datetime, timedelta

def is_leap_year(year):
    return year % 4 == 0 and (year % 100 != 0 or year % 400 == 0)

def random_birthday(year):
    if is_leap_year(year):
        days_in_year = 366
    else:
        days_in_year = 365
    start_date = datetime(year,1,1)
    random_days = random.randint(0, days_in_year -1)

    random_date = start_date + timedelta(days=random_days)

    return random_date.strftime('%d.%m.%Y')


if __name__ == "__main__":
    print(random_birthday(2022))
    print(random_birthday(2023))
    print(random_birthday(1912))





