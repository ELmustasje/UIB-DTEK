from datetime import datetime, timedelta

def first_friday_13th_after(date):
    year = date.year
    month = date.month
    
    if date.day >= 13:
        month +=1
        if month > 12:
            month = 1
            year += 1

    while True:
        if month > 12:
            month = 1
            year +=1

        p_friday_13th = datetime(year, month, 13)

        if p_friday_13th.weekday() == 4 and p_friday_13th > date:
            return p_friday_13th
        
        month +=1

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
