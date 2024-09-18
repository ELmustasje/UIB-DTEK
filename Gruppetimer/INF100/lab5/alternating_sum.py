def alternate_sign_sum(summands):
    sum = 0
    for i in range(len(summands)):
        if i % 2 == 0:
            sum += summands[i]
        else:
            sum -= summands[i]
    return sum


a = alternate_sign_sum([1, 2, 3, 4, 5])
print(a)
