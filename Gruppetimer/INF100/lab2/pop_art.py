from uib_inf100_graphics.simple import *
from random import randint

farger = ["blue", "pink", "red", "cyan", "purple", "green1", "magenta"]


def lagSirkel(x, y):
    canvas.create_oval(x, y, x + 150, y + 150, fill=farger[randint(0, len(farger) - 1)])


lagSirkel(10, 10)
lagSirkel(250, 10)
lagSirkel(250, 250)
lagSirkel(10, 250)
display(canvas)
