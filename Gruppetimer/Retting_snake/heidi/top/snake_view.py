def get_color (value):
    if value == 0:
        color = fill="#ACE1aF"
    elif value > 0:
        color = fill="#4B6F44"
    elif value < 0:
        color = fill="red" 
    return color

def draw_board (canvas, x1, y1, x2, y2, board, info_mode):
    rows = len(board)
    cols = len (board [0])
    rectangle_width = (x2-x1) / cols
    rectangle_height = (y2-y1) / rows

    for row in range (rows):
        for col in range (cols):
            rectangle_left = x1 + col *rectangle_width
            rectangle_top = y1 + row * rectangle_height
            rectagle_right = rectangle_left + rectangle_width
            rectangle_bottom = rectangle_top + rectangle_height
            color = get_color(board[row][col])
            canvas.create_rectangle (rectangle_left, rectangle_top, rectagle_right, rectangle_bottom, fill=color)
            
            if info_mode == True:
                rectangle_mid_x = (rectangle_left + rectagle_right) / 2
                rectangle_mid_y = (rectangle_top + rectangle_bottom) / 2
                canvas.create_text (rectangle_mid_x, rectangle_mid_y, text=f'{row}, {col}\n{board[row][col]}')

               

if __name__ == '__main__':
    from uib_inf100_graphics.simple import canvas, display

    test_board = [
        [1, 2, 3, 0, 5, 4,-1,-1, 1, 2, 3],
        [0, 4, 0, 7, 0, 3,-1, 0, 0, 4, 0],
        [0, 5, 0, 8, 1, 2,-1,-1, 0, 5, 0],
        [0, 6, 0, 9, 0, 0, 0,-1, 0, 6, 0],
        [0, 7, 0,10,11,12,-1,-1, 0, 7, 0],
    ]

    draw_board(canvas, 25, 25, 375, 375, test_board, True)
    display(canvas)
