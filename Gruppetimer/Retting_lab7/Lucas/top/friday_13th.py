from datetime import date, timedelta, datetime


def first_friday_13th_after(day):
    day
    end = datetime(5000, 12, 31)
    one_day = timedelta(days=1)
    day += one_day
    while day < end:
        if day.weekday() == 4 and day.day == 13:
            return day
        day += one_day

    result = day
    return result

