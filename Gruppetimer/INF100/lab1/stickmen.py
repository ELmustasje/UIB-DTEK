from uib_inf100_graphics.simple import canvas, display


def createman(x, y):
    canvas.create_oval(x, y, x + 100, y + 100)
    canvas.create_line(x + 50, y + 100, x + 50, y + 250)
    canvas.create_line(x - 20, y + 120, x + 120, y + 120)
    canvas.create_line(x + 50, y + 250, x - 10, y + 300)
    canvas.create_line(x + 50, y + 250, x + 110, y + 300)


createman(50, 50)
createman(250, 50)

display(canvas)
