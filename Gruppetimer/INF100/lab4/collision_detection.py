from math import dist


def rectangles_overlap(x1, y1, x2, y2, x3, y3, x4, y4):
    venstre = min(x1, x2)
    hoyre = max(x1, x2)
    oppe = max(y1, y2)
    nede = min(y1, y2)

    punkt = [x3, y3, x4, y4]
    for punkt in punkt:
        if punkt >= venstre and punkt <= hoyre and punkt >= nede and punkt <= oppe:
            return True
    return False


print("Tester rectangles_overlap... ", end="")
print(rectangles_overlap(0, 0, 5, 5, 3, 6, 5, 8))  # Utenfor
print("OK")
