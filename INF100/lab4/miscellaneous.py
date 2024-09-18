from math import isclose, dist


def multiply_5_minus_pi(my_number):
    my_number = (my_number * 5) - 3.14
    return my_number


def shout(text):
    text = text + "!"
    return text


def name_age(name, gender, age):
    return name + " er " + gender + " og er " + str(age) + " år gammel."


def kinetic_energy(m, v):
    return 0.5 * m * v**2


def count_letters(s, t):
    count = 0
    for i in s:
        if i in t:
            count += 1
    return count


def distance(x1, y1, x2, y2):
    return dist((x1, y1), (x2, y2))


def point_in_rectangle(x1, y1, x2, y2, xp, yp):
    venstre = min(x1, x2)
    høyre = max(x1, x2)
    oppe = max(y1, y2)
    nede = min(y1, y2)
    if xp >= venstre and xp <= høyre and yp >= nede and yp <= oppe:
        return True
    else:
        return False
