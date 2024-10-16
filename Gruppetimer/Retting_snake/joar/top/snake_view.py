from uib_inf100_graphics.helpers import text_in_box 

def colour(x):
    if x < 0:
        return 'cyan'
    elif x > 0:
      return 'orange'
    else:
        return 'lightgray'

def draw_board(canvas,x1,y1,x2,y2,board,info_mode):
    width = (x2-x1)/len(board[0])
    height = (y2-y1)/len(board)

    for i, y in enumerate(board):
        for m, x in enumerate(y):
            canvas.create_rectangle(x1+width*m,y1+height*i,width*m+width+x1,height*i+height+y1, fill = colour(x))
            if info_mode == True:
                text_in_box(canvas,x1+width*m,y1+height*i,width*m+width+x1,height*i+height+y1, f"{i},{m}\n{x}", padding = 5)


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
