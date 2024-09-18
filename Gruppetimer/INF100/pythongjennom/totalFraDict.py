Gjester = {
    "thomas": {"epple": 1, "banan": 2},
    "ole": {"vodka": 1, "epple": 2},
    "lars": {"mein kamf": 1, "tofu": 2},
    "sindre": {"mein kamf": 3, "banan": 3, "vodka": 4},
}


def total(Gjester):
    items = {}
    for i in Gjester:
        for k in Gjester[i]:
            if k not in items:
                items[k] = Gjester[i][k]
                continue
            else:
                items[k] += Gjester[i][k]
    for i in items:
        print(i, items[i])
    return items


total(Gjester)
