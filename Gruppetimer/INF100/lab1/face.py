from uib_inf100_graphics.simple import canvas, display

canvas.create_oval(50, 50, 350, 350, fill="#FFEFDB")
##øyne
canvas.create_oval(120, 120, 170, 170, fill="white")
canvas.create_oval(230, 120, 280, 170, fill="white")
##munn
canvas.create_arc(
    160, 250, 235, 275, start=0, extent=180, outline="black", width=2, fill="black"
)
##nese
canvas.create_polygon(
    190, 175, 190, 225, 230, 225, fill="#8B7D6B", outline="black", width=2
)
display(canvas)
