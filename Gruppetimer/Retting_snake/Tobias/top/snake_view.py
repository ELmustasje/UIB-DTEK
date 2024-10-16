#Funksjon for og tegne selve rutenettet
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
            cell_mid_x = (cell_left + cell_right) / 2
            cell_mid_y = (cell_top + cell_bottom) / 2

            canvas.create_rectangle(
                cell_left, cell_top, cell_right, cell_bottom,
                fill=color
            )

            if info_mode == True:
                canvas.create_text(
                cell_mid_x, cell_mid_y,
                text=f'{row},{col}\n{board[row][col]}',
                font='Arial 10',)

#Hjelpe funksjon for farger i rutenett
def get_color(value):
    if value == 0:
        return('Green') #Brett
    elif value < 0:
        return('Red')  #Eplet
    elif value > 0:
        return('#D0FF14') #Slangen


