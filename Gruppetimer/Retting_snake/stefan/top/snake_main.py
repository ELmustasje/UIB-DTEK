from snake_view import draw_board
import random

def app_started(app):
    # Modellen.
    # Denne funksjonen kalles én gang ved programmets oppstart.
    # Her skal vi __opprette__ variabler i som behøves i app.
    
    app.width = int(app._root.winfo_screenwidth() * 0.8)
    app.height = int(app._root.winfo_screenheight() * 0.8)
    app._root.geometry(f'{app.width}x{app.height}') 
    
    app.direction = 'east'
    app.info_mode = True
    app.board = [
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 2, 3, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0], 
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
]
    app.snake_size = 3
    app.head_pos = (3, 4)
    app.state = 'active'
    app.apples_eaten = 0
    add_apple_at_random_location(app.board)
    app.timer_delay = 200

def timer_fired(app):
    # En kontroller.
    # Denne funksjonen kalles ca 10 ganger per sekund som standard.
    # Funksjonen kan __endre på__ eksisterende variabler i app.
    if not app.info_mode and app.state == 'active':
        move_snake(app)



def key_pressed(app, event):
    # En kontroller.
    # Denne funksjonen kalles hver gang brukeren trykker på tastaturet.
    # Funksjonen kan __endre på__ eksisterende variabler i app.
    
    if event.key == 'Up':
        app.direction = 'north'
    elif event.key == 'Down':
        app.direction = 'south'
    elif event.key == 'Left':
        app.direction = 'west'
    elif event.key == 'Right':
        app.direction = 'east'

    if app.state == 'active':
        if event.key == 'Space' and app.info_mode:
            move_snake(app)
        elif event.key == 'i':
                app.info_mode = not app.info_mode
    elif app.state == 'gameover':
        if event.key == 'i':
            app.info_mode = not app.info_mode
        elif event.key == 'Enter':  
            start_game(app)


def redraw_all(app, canvas):
    # Visningen.
    # Denne funksjonen tegner vinduet. Funksjonen kalles hver gang
    # modellen har endret seg, eller vinduet har forandret størrelse.
    # Funksjonen kan __lese__ variabler fra app, men har ikke lov til
    # å endre på dem.

    margin = 100

    x1 = margin + 250
    y1 = margin
    x2 = app.width - margin
    y2 = app.height - margin
    
    draw_board(canvas, x1, y1, x2, y2, app.board, app.info_mode)

    if app.state == 'active':
        display_score(canvas, app.apples_eaten)    
        if app.info_mode == True:
            text = f'{app.head_pos=} {app.snake_size=} {app.direction=} {app.state=}'
            canvas.create_text(app.width/2, 13, text = text, font='Arial 10')
    elif app.state == 'gameover':
        canvas.delete("all")
        canvas.create_text(app.width / 2, app.height / 2 - 20, text="Game Over", font="Arial 40 bold", fill="red")
        Score = f'Score: {app.apples_eaten}'
        canvas.create_text(app.width / 2, app.height / 2 + 40, text=Score, font="Arial 20", fill="black")
        restart = "Press Enter to restart"
        canvas.create_text(app.width / 2, app.height / 2 + 80, 
                           text=restart, font="Arial 20", fill="blue", tags="restart_button")


def get_next_head_position(app):

    row, col = app.head_pos

    direction = app.direction

    if direction == 'east':
        next_pos = (row, col + 1)
    elif direction == 'west':
        next_pos = (row, col - 1)
    elif direction == 'north':
        next_pos = (row - 1, col)
    elif direction == 'south':
        next_pos = (row + 1, col)

    return next_pos

def subtract_one_from_all_positives(app):
    for row in range(len(app.board)):
        for col in range(len(app.board[0])):
            if app.board[row][col] > 0:
                app.board [row][col] -= 1


def move_snake(app):
    
    new_head_pos = get_next_head_position(app)

    if not is_legal_move(new_head_pos, app.board):
        app.state = 'gameover'
        return

    head_row, head_col = new_head_pos
    if app.board[head_row][head_col] == -1:
        app.snake_size += 1
        app.apples_eaten += 1
        app.timer_delay -= 5
        add_apple_at_random_location(app.board)
    else:
        subtract_one_from_all_positives(app)

    app.head_pos = new_head_pos
    
    app.board[head_row][head_col] = app.snake_size
        
def add_apple_at_random_location(grid):
    empty_positions = [(row, col) for row in range(len(grid)) 
                       for col in range(len(grid[0])) if grid[row][col] == 0]
    
    if empty_positions:
        row, col = random.choice(empty_positions)
        grid[row][col] = -1 

def is_legal_move(pos, board):
    row, col = pos
    if row <0 or row >= len(board) or col < 0 or col >= len(board[0]):
        return False
    
    if board[row][col] > 0:
        return False
        
    return True

def start_game(app):
    reset_game(app)  
    app.state = 'active' 

def reset_game(app):
    app.board = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 2, 3, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0], 
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]
    app.direction = 'east'
    app.snake_size = 3
    app.head_pos = (3, 4)
    app.apples_eaten = 0 
    app.timer_delay = 200  
    add_apple_at_random_location(app.board)

def display_score(canvas, score):
    canvas.create_text(50, 10, text=f'Score: {score}', font='Arial 16 bold', fill='black')



if __name__ == '__main__':
    from uib_inf100_graphics.event_app import run_app
    run_app(width=500, height=400, title='Snake')
