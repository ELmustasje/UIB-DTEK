def app_started(app):
    app.direction = 'east'
    app.info_mode = True
    app.board = [
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0,-1, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 2, 3, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]
    app.snake_size = 3
    app.head_pos = (3,4)
    app.state = 'active'

def is_legal_move(y,x, board):
    if (len(board[0])-1 >= x >= 0) and 0 <= y <= len(board)-1:
        print('a')
        if board[y][x] <= 0:
            return True
        else:
            print('b')
            return False
    else:
        print('c')
        return False

def move_snake(app):
    y,x = app.head_pos
    if app.direction == 'north':
        y -= 1
    elif app.direction == 'south':
        y += 1
    elif app.direction == 'west':
        x -= 1
    elif app.direction == 'east':
        x += 1
    
    if is_legal_move(y,x,app.board) == False:
        app.state = 'gameover'
        return
    else:
        app.state = 'active'
    
    if app.board[y][x] < 0:
        apple(app, y, x)
    else:
        app.head_pos = (y,x)

        for i in app.board:
            for koor, verdi in enumerate(i):
                if verdi > 0:
                    i[koor] -= 1
    
        app.board[y][x] = app.snake_size
            
def apple(app, y, x):
    app.snake_size +=1
    app.head_pos = (y,x)    
    app.board[y][x] = app.snake_size

    row = random.randrange(7)
    col = random.randrange(9)
    while app.board[row][col] > 0:
        row = random.randrange(7)
        col = random.randrange(9)
    
    app.board[row][col] = -1

def timer_fired(app):
    if app.info_mode == False and app.state == 'active':
        move_snake(app)
    
    app.timer_delay = 200

def key_pressed(app, event):

    if event.key == 'i':
        app.info_mode = not app.info_mode
    
    if app.state == 'active':
        #if event.key == 'Space':
            #move_snake(app)
    
        for knapp, retning in [['Up','north'],['Down','south'],['Left','west'],['Right','east']]:
            if event.key == knapp:
                app.direction = retning
    


def redraw_all(app, canvas):
    if app.info_mode == True:
        text_in_box(canvas, 0, 0, 500, 20, f"app.direction={app.direction}, app.snake_size={app.snake_size}, app.head_pos={app.head_pos}")
    
    if app.state == 'active':
        draw_board(canvas, 25, 25, 475, 375, app.board, app.info_mode)
    else:
        text_in_box(canvas, 100,100,400,200,'Game Over')

if __name__ == '__main__':
    import random
    from uib_inf100_graphics.event_app import run_app
    from uib_inf100_graphics.helpers import text_in_box
    from snake_view import draw_board
    run_app(width=500, height=400, title='Snake')

