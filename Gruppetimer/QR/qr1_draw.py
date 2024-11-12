def draw_qr(canvas, x_left, y_top, size, qr):
    n_cr = len(qr)
    cr = size / n_cr
    color = [
        "White",
        "Black"
    ]
    for y in range(len(qr)):
        for x in range(len(qr[y])):
            canvas.create_rectangle(x_left+x*cr, y_top+y*cr,
                                    x_left+(x+1)*cr, y_top+(y+1)*cr,
                                    fill=color[qr[y][x]],
                                    outline=""
                                    )

def display(matrix):
    from uib_inf100_graphics.simple import canvas, display as dsp
    canvas.create_rectangle(0, 0, 400, 400, fill='white', outline='')
    draw_qr(canvas, 25, 25, 350, matrix)
    dsp(canvas)

if __name__ == '__main__':
    sample_grid = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1],
        [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0],
        [0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0],
        [0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0],
        [0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        [1, 1, 1, 0, 1, 1, 0, 1, 1, 0, 1, 1, 1],
        [0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0],
        [0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0],
        [0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 0],
        [0, 1, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]
    
    display(sample_grid)
