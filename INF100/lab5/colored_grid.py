from uib_inf100_graphics.simple import canvas, display


def draw_grid(canvas, x1, y1, x2, y2, colors):
    diffx = max(x1, x2) - min(x1, x2)
    diffy = max(y1, y2) - min(y1, y2)
    y = y1
    x = x1
    print(len(colors))
    for i in range(len(colors)):
        for g in range(len(colors[i])):
            canvas.create_rectangle(
                x,
                y,
                x + diffx / len(colors[i]),
                y + diffy / len(colors),
                fill=colors[i][g],
            )
            x += diffx / len(colors[i])
        x = x1
        y += diffy / len(colors)

    return canvas
