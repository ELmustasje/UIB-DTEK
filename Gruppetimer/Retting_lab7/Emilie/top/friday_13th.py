from datetime import datetime, timedelta
def is_friday_13th(date):
    return date.day == 13 and date.weekday() == 4

def first_friday_13th_after(date):
    date += timedelta(days=1)
    while not is_friday_13th(date):
        date += timedelta(days=1)
    return date

def test_first_friday_13th_after():
    print('Tester first_friday_13th_after... ', end='')
    # Test 1
    result = first_friday_13th_after(datetime(2022, 10, 24))
    assert (2023, 1, 13) == (result.year, result.month, result.day)
    # Test 2
    result = first_friday_13th_after(datetime(2023, 1, 13))
    assert (2023, 10, 13) == (result.year, result.month, result.day)
    # Test 3
    result = first_friday_13th_after(datetime(1950, 1, 1))
    assert (1950, 1, 13) == (result.year, result.month, result.day)
    print('OK')

if __name__ == "__main__":
    test_first_friday_13th_after()