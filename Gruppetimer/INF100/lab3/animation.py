from uib_inf100_graphics.simple import *

x = 100
y = 50
xspeed = 7
yspeed = 5

while True:
    canvas.create_oval(x, y, x + 50, y + 50, fill="red")
    canvas.create_rectangle(0, 0, 400, 400)
    x += xspeed
    y += yspeed
    if x > 350 or x < 0:
        xspeed = xspeed * -1
    if y > 350 or y < 0:
        yspeed = yspeed * -1
    display(canvas)
