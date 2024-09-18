from uib_inf100_graphics.simple import canvas, display

canvas.create_rectangle(50, 50, 200, 200, fill="green")
canvas.create_oval(100, 100, 250, 250, fill="yellow")
canvas.create_arc(150, 150, 300, 300, fill="red", start=90, extent=300)
canvas.create_polygon(200, 200, 200, 380, 350, 350, fill="blue")


display(canvas)
