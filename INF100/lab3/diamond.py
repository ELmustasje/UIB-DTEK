sider = int(input("Sidelengde: \n"))


def createdia(sider):
    ant = 1
    max = (2 * sider) - 1
    for i in range(sider):
        print((" " * (int((max - 1) / 2) - i) + "X" * ant))
        ant += 2
    ant -= 2
    for i in range(sider - 1):
        ant -= 2
        print((" " * (i + 1) + "X" * ant))


createdia(sider)
