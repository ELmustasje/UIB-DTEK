import random
from snake_view import draw_board


def app_started(app):
    app.direction = 'east'
    app.info_mode = True
    app.state = 'active'
    app.timer_delay = 200   
    
    app.board = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, -1, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 2, 3, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]
    app.snake_size = 3  
    app.head_pos = (3, 4) 

def timer_fired(app):
    if not app.info_mode and app.state == 'active':
        move_snake(app)

def key_pressed(app, event):
    if app.state == 'active':
        if event.key == 'Up':
            app.direction = 'north'
        elif event.key == 'Down':
            app.direction = 'south'
        elif event.key == 'Left':
            app.direction = 'west'
        elif event.key == 'Right':
            app.direction = 'east'
        elif event.key == 'Space':
            move_snake(app)
    if event.key == 'i':
        app.info_mode = not app.info_mode

def is_legal_move(pos, board):
    row, col = pos
    if 0 <= row < len(board) and 0 <= col < len(board[0]):
        return board[row][col] == 0 or board[row][col] == -1
    return False


def get_next_head_position(head_pos, direction):
    row, col = head_pos
    if direction == 'north':
        return (row - 1, col)
    elif direction == 'south':
        return (row + 1, col)
    elif direction == 'west':
        return (row, col - 1)
    elif direction == 'east':
        return (row, col + 1)
    return head_pos


def subtract_one_from_all_positives(grid):
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if grid[row][col] > 0:
                grid[row][col] -= 1

#kilden jeg har brukt for å skrive funksjonen under er hentet fra docs.python, 2024
#https://docs.python.org/3/library/random.html#functions-for-integers
def add_apple_at_random_location(grid):
    while True:
        row = random.randint(0, len(grid) - 1)    
        col = random.randint(0, len(grid[0]) - 1)
        if grid[row][col] == 0:
            grid[row][col] = -1 
            return  


def move_snake(app):
    next_head_pos = get_next_head_position(app.head_pos, app.direction)
    if not is_legal_move(next_head_pos, app.board):
        app.state = 'gameover' 
        return    
    next_row, next_col = next_head_pos  
    if app.board[next_row][next_col] == -1:
        app.snake_size += 1
        add_apple_at_random_location(app.board)
    else:
        subtract_one_from_all_positives(app.board)
    
    app.head_pos = next_head_pos
    app.board[next_row][next_col] = app.snake_size


def redraw_all(app, canvas):
    if app.state == 'gameover':
        canvas.create_text(app.width / 2, app.height / 2, text='Game Over', fill='red')
    else:
        if app.info_mode:
            info_text = f'{app.direction=}, {app.snake_size=}, {app.head_pos=}, {app.state=}'
            canvas.create_text(5, 5, anchor='nw', text=info_text, fill='black')
        
        x1, y1 = 25, 25
        x2, y2 = app.width - 25, app.height - 25
        draw_board(canvas, x1, y1, x2, y2, app.board, app.info_mode)


if __name__ == '__main__':
    from uib_inf100_graphics.event_app import run_app
    run_app(width=500, height=400, title='Snake')
