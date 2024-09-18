import random, copy, time, os

width = 150
heigth = 26

nextCells = []

for i in range(width):
    col = []
    for x in range(heigth):
        if random.randint(0, 1) == 0:
            col.append("#")
        else:
            col.append(" ")
    nextCells.append(col)
a = True
while a == True:
    currentCells = copy.deepcopy(nextCells)

    for y in range(heigth):
        for x in range(width):
            print(currentCells[x][y], end="")
        print()

    for y in range(heigth):
        for x in range(width):
            naboer = 0
            leftCoord = (x - 1) % width
            rightCoord = (x + 1) % width
            aboveCoord = (y - 1) % heigth
            belowCoord = (y + 1) % heigth

            if currentCells[leftCoord][aboveCoord] == "#":
                naboer += 1
            if currentCells[x][aboveCoord] == "#":
                naboer += 1
            if currentCells[rightCoord][aboveCoord] == "#":
                naboer += 1
            if currentCells[leftCoord][y] == "#":
                naboer += 1
            if currentCells[rightCoord][y] == "#":
                naboer += 1
            if currentCells[leftCoord][belowCoord] == "#":
                naboer += 1
            if currentCells[x][belowCoord] == "#":
                naboer += 1
            if currentCells[rightCoord][belowCoord] == "#":
                naboer += 1

            if currentCells[x][y] == "#" and (naboer == 2 or naboer == 3):
                nextCells[x][y] = "#"
            elif currentCells[x][y] == " " and naboer == 3:
                nextCells[x][y] = "#"
            else:
                nextCells[x][y] = " "

    os.system("CLS")
