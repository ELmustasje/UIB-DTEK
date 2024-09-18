from uib_inf100_graphics.simple import canvas, display


def draw_multicolored_flag(canvas, x1, y1, x2, y2, colors):
    diff = max(x1, x2) - min(x1, x2)
    bredde = abs(x2 - x1)
    hoyde = abs(y2 - y1)

    ant_striper = len(colors)

    x = x1
    for i in range(ant_striper):
        canvas.create_rectangle(
            x, y1, x + diff / ant_striper, y2, fill=colors[i], outline=""
        )
        x += diff / ant_striper

    return canvas
