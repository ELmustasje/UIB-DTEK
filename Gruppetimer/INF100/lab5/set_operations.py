def no_duplicates(a):
    nodup = []
    for i in a:
        if not i in nodup:
            nodup.append(i)
    return nodup


def set_difference(setA, setB):
    diff = []
    for i in setA:
        if not i in setB:
            diff.append(i)
    return diff


def set_union(setA, setB):
    fullset = setA + setB
    return no_duplicates(fullset)


def set_intersection(setA, setB):
    same = []
    for i in setA:
        if i in setB:
            same.append(i)
    return same
