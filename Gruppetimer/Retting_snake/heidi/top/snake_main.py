from snake_view import draw_board

def app_started(app):

    app.state = "start"
    app.info_mode = False
    app.pause_mode = False
    app.difficulty = "normal"

    app.board = opprett_dynamisk_brett (10, 14)
    app.direction = "east"
    app.snake_size = 1
    app.head_pos = (3, 4)
    app.points = 0
    

def timer_fired(app):

    if app.state == "start":
        if app.difficulty == "lett":
            app.timer_delay = 150
        if app.difficulty == "normal":
            app.timer_delay = 100
        if app.difficulty == "vanskelig":
            app.timer_delay = 50

    
    if app.info_mode == False:
        if app.state == "active":
            if app.pause_mode == False:
                move_snake (app)
    

def key_pressed(app, event):

    if app.state == "start":
        if event.key == "s":
            app.state = "active"
        if event.key == "r":
            app.state = "rules"
        if event.key == "1":
            app.difficulty = "lett"
        if event.key == "2":
            app.difficulty = "normal"
        if event.key == "3":
            app.difficulty = "vanskelig"

    if app.state == "rules":
        if event.key == "b":
            app.state = "start"

    if app.state == "gameover":
        if event.key == "i":
            if app.info_mode == True:
                app.info_mode = False
            elif app.info_mode == False:
                app.info_mode = True
        if event.key == "s":
            app_started (app)
    
        
    if app.state == "active":
        if event.key == "i":
            if app.info_mode == True:
                app.info_mode = False
            elif app.info_mode == False:
                app.info_mode = True
        if event.key == "p":
            if app.pause_mode == False:
                app.pause_mode = True
            elif app.pause_mode == True:
                app.pause_mode = False
        

        if event.key == "Up":
            app.direction = "north"
        if event.key == "Down":
            app.direction = "south"
        if event.key == "Left":
            app.direction = "west"
        if event.key == "Right":
            app.direction = "east"
        if event.key == "Space":
            move_snake (app)


def redraw_all(app, canvas):
    # Denne funksjonen tegner vinduet.
    if app.info_mode == True:
        ax, ay = 250,0
        canvas.create_text(ax, ay, anchor="n", text=f'{app.head_pos=} {app.snake_size=} {app.direction=} {app.state=}', font=("", 10, ""))

    if app.pause_mode == True:
        canvas.create_rectangle (10, 10, app.width -10, app.height-10, outline="Black", fill="#B284BE")

    app_middel_x = app.width/2
    app_middel_y = app.height/2
    game_width = app.width - 50
    game_height = app.height - 50
    game_x2 = app.width - 25
    game_x1 = game_x2 - game_width
    game_y2 = app.height - 25
    game_y1 = game_y2 - game_height
    
    if app.state == "start":
        canvas.create_rectangle (10, 10, app.width -10, app.height-10, outline="Black", fill="#B284BE")
        canvas.create_text (app_middel_x, app_middel_y*0.5, text="Welcome to Snake", font = ("", 35, ""))
        canvas.create_text (app_middel_x, app_middel_y*0.8, text="Start: s", font =("", 20, ""))
        canvas.create_text (app_middel_x*0.4, app_middel_y*1.5, text="Rules: r", font =("", 15, ""))
        canvas.create_text (app_middel_x*1.4, app_middel_y*1.5, text="Difficulty settings: \nEasy: 1\nNormal: 2 \nDifficult: 3", font=("", 15,""))
    
    if app.state == "rules":
        canvas.create_rectangle (10, 10, app.width-10, app.height-10, outline="Black", fill="#B284BE")
        canvas.create_text (app_middel_x, app_middel_y, text="Rules Snake:\nYou controll the snake with the keyarrows (easy right?)\nEat apples to grow bigger (apples are good for you!)\nGame over if you crash into the walls or if you try to eat yourself\n(Surprise! Eating yourself is bad)\n\n\n\nInfo\nPush i when playing for information mode\nPush p to pause game\nPush b to get back to start", font=("", 10, ""))
    
    if app.state == "active":
        draw_board (canvas, game_x1, game_y1, game_x2, game_y2, app.board, app.info_mode)
    
    if app.state == "gameover":
        points = (app.points)
        if app.difficulty == "lett":
            points = points * 1
        elif app.difficulty == "normal":
            points = points * 2
        elif app.difficulty == "vanskelig":
            points = points * 3
        canvas.create_rectangle (10, 10, app.width-10, app.height-10, outline="Black", fill="#B284BE")
        canvas.create_text (app_middel_x, app_middel_y*0.5, text="Game Over", font= ("", 50, ""))
        canvas.create_text (app_middel_x, app_middel_y*1.2, text=f'You got {points} points', font=("", 20, ""))
        canvas.create_text (app_middel_x, app_middel_y*1.6, text="Don't panic! You can play again. Push s", font=("", 15, ""))
    

def get_next_head_position (head_pos, direction): 
    row, col = head_pos
    if direction == "east":
        col += 1
    elif direction == "west":
        col -= 1
    elif direction == "north":
        row -= 1
    elif direction == "south":
        row += 1
    return (row, col)

def subtract_one_from_all_positives (grid):
    rows = len(grid)
    cols = len(grid[0])
    for row in range(rows):
        for col in range(cols):
            if grid[row][col] > 0:
                grid[row][col] -= 1

def add_apple_at_random_location(grid):
    import random
    rows, cols = len(grid), len(grid[0])
    legal_apple_positions = []
    for row in range (rows):
        for col in range (cols):
            if grid[row][col] == 0:
                legal_apple_positions.append((row, col))
    
    random_num_a, random_num_b = random.choice(legal_apple_positions)
    grid[random_num_a][random_num_b] = -1 

def opprett_dynamisk_brett (rows, cols):
    dynamisk_brett = [[0]*cols for _ in range(rows)]
    import random
    rows, cols = len(dynamisk_brett), len(dynamisk_brett[0])
    legal_snake_start_positions = []
    for row in range (rows):
        for col in range (cols):
            if dynamisk_brett[row][col] == 0:
                legal_snake_start_positions.append((row, col))
    random_num_a, random_num_b = random.choice(legal_snake_start_positions)
    dynamisk_brett[random_num_a][random_num_b] = 1
    
    legal_apple_start_positions = []
    for row in range(rows):
        for col in range(cols):
            if dynamisk_brett[row][col] == 0:
                legal_apple_start_positions.append((row,col))
    random_num_c, random_num_d = random.choice(legal_apple_start_positions)
    dynamisk_brett[random_num_c][random_num_d] = -1

    return dynamisk_brett


def is_legal_move (pos, board):
    rows, cols = len(board), len(board[0])
    x, y = pos
    if x in range (rows):
        if y in range (cols):
            if board[x][y] <= 0:
                return True
            else:
                return False
        else:
            return False
    else:
        return False
    
def move_snake (app):
    app.head_pos = get_next_head_position (app.head_pos, app.direction)
    if is_legal_move (app.head_pos, app.board) == False:
        app.state = "gameover"
        return
    row, col = app.head_pos
    points_counter = 0
    apple = -1
    if app.board[row][col] == apple:
        app.snake_size += 1
        app.points += 1
        add_apple_at_random_location (app.board)
    else:
        app.head_pos 
        subtract_one_from_all_positives (app.board)
    app.board[row][col] = app.snake_size
    
    

if __name__ == '__main__':
    from uib_inf100_graphics.event_app import run_app

    run_app(width=500, height=400, title='Snake')
