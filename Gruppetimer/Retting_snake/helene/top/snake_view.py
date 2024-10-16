

def draw_board(canvas, x1, y1, x2, y2, board, info_mode):
    rader = (len(board))
    kolonner = (len(board[0]))
    cell_width = (x2-x1)/ kolonner
    cell_height = (y2-y1) / rader
    color = 'black'

    for rad in range (rader):
        for kol in range (kolonner):
            cell_left = x1 + kol *cell_width
            cell_right = cell_left +  cell_width
            cell_top = y1 + rad * cell_height
            cell_bottom = cell_top  + cell_height

            if board [rad][kol] == 0:
                color= 'green'
            elif board [rad] [kol] > 0:
                color = 'cyan'
            elif board [rad] [kol] < 0:
                color = 'red'

            canvas.create_rectangle(cell_left, cell_top, cell_right, cell_bottom, fill=color)

            if info_mode == True:
                text_x = (cell_left + cell_right) / 2
                text_y = (cell_top + cell_bottom) / 2
                text = f'{rad}, {kol} \n{board[rad][kol]}'
                canvas.create_text(text_x, text_y, text=text, anchor = 'center')
                        
if __name__ == '__main__':
    from uib_inf100_graphics.simple import canvas, display

  