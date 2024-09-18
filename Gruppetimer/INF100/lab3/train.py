from uib_inf100_graphics.simple import *

ant = input("Hvor mange vinduer skal toget ha? ")


def tegntog(ant):
    ant = int(ant)
    startx = 10
    starty = 10
    dist = 5
    høyde = 10
    bredde = 5

    canvas.create_rectangle(startx, starty, ((ant + 1) * 10) + dist, starty + 20)
    for i in range(ant):
        canvas.create_rectangle(
            startx + dist + (i * 10),
            starty + dist,
            startx + dist + bredde + (i * 10),
            starty + dist + høyde,
        )
    display(canvas)


tegntog(ant)
