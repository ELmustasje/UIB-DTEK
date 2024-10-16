
def get_color(value):
    
    if value > 0:
        return 'orange'
    elif value == 0:
        return 'lightgray'
    else:
        return 'cyan'

def draw_board(canvas, x1, y1, x2, y2, board, info_mode):
    
    rows = len(board)
    cols = len(board[0])
    cell_width = (x2 - x1) / cols
    cell_height = (y2 - y1) / rows

    for row in range(rows):
        for col in range(cols):
            cell_left = x1 + col * cell_width
            cell_top = y1 + row * cell_height
            cell_right = cell_left + cell_width
            cell_bottom = cell_top + cell_height

            color = get_color(board[row][col])

            canvas.create_rectangle(cell_left, cell_top, cell_right, cell_bottom, fill=color)

            
            if info_mode:
                cell_mid_x = (cell_left + cell_right) / 2
                cell_mid_y = (cell_top + cell_bottom) / 2
                text = f'{row},{col}\n{board[row][col]}'
                canvas.create_text(cell_mid_x, cell_mid_y, text=text, fill='black')





if __name__ == '__main__':
    from uib_inf100_graphics.simple import canvas, display

    test_board = [
        [1, 2, 3, 0, 5, 4, -1, -1, 1, 2, 3],
        [0, 4, 0, 7, 0, 3, -1, 0, 0, 4, 0],
        [0, 5, 0, 8, 1, 2, -1, -1, 0, 5, 0],
        [0, 6, 0, 9, 0, 0, 0, -1, 0, 6, 0],
        [0, 7, 0, 10, 11, 12, -1, -1, 0, 7, 0],
    ]

    draw_board(canvas, 25, 80, 375, 320, test_board, True)
    display(canvas)
