import itertools


def smallest_absolute_difference(num):
    diff = 10**8
    for i in range(len(num) - 1):
        for g in range(i + 1, len(num)):
            if abs(num[i] - num[g]) < diff:
                diff = abs(num[i] - num[g])
    return diff
