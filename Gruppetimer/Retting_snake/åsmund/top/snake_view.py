def draw_board(canvas,x1,y1,x2,y2,board,info_mode):
    antall_rader = len(board)
    delta_y = (y2-y1)/antall_rader  #hentet linje fra tidligere oppgaver, gjelder osso tilsvarende linjer
    for r in range(antall_rader):
        antall_kolonner = len(board[r])
        delta_x = (x2-x1)/antall_kolonner   #
        for k in range(antall_kolonner):
            
            #hentet fra tidligere oppgave
            start_x = x1 + delta_x*k
            slutt_x = start_x + delta_x
            start_y = y1 + delta_y*r
            slutt_y = start_y + delta_y
            canvas.create_rectangle(start_x,start_y, slutt_x, slutt_y, fill=get_color(board[r][k]))
    
    if info_mode:
        antall_rader = len(board)
        delta_y = (y2-y1)/antall_rader  #
        for r in range(len(board)):
            antall_kolonner = len(board[r])
            delta_x = (x2-x1)/antall_kolonner   #
            for k in range(len(board[r])):
                canvas.create_text(
                    (x1 + delta_x*k) + delta_x/2,
                    (y1 + delta_y*r) + delta_y/2,
                    text=(f'{r}, {k}\n{board[r][k]} '),
                    font='Verdana 8')
def get_color(tall):
    if   tall == 0:
        farge = 'lightgray'
    elif tall >  0:
        farge = 'green'
    elif tall <  0:
        farge = 'red'
    else:
        assert False
    return farge

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


    