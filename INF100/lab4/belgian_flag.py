from uib_inf100_graphics.simple import canvas, display


def draw_belgian_flag(canvas, x1, y1, x2, y2):
    diff = max(x1, x2) - min(x1, x2)
    bredde = abs(x2 - x1)
    hoyde = abs(y2 - y1)

    svart_stripe_start = x1
    svart_stripe_slutt = svart_stripe_start + diff / 3
    gul_stripe_start = svart_stripe_slutt
    gul_stripe_slutt = gul_stripe_start + diff / 3
    rod_stripe_start = gul_stripe_slutt
    rod_stripe_slutt = rod_stripe_start + diff / 3

    canvas.create_rectangle(
        svart_stripe_start, y1, svart_stripe_slutt, y2, fill="BLACK"
    )
    canvas.create_rectangle(
        gul_stripe_start, y1, gul_stripe_slutt, y2, fill="YELLOW", outline="YELLOW"
    )
    canvas.create_rectangle(
        rod_stripe_start, y1, rod_stripe_slutt, y2, fill="RED", outline="RED"
    )

    return canvas
