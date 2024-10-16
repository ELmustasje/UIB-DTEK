def app_started(app):
    app.direction = "east"
    app.info_mode = True
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
    app.apple_pos = (1, 6)
    app.state = "welcome"
    app.timer_delay = 500
    app.score = 0


def get_next_head_position(head_pos, direction):
    row, col = head_pos
    if direction == "east":
        col += 1
    if direction == "west":
        col -= 1
    if direction == "north":
        row -= 1
    if direction == "south":
        row += 1
    return (row, col)


def subtract_one_from_all_positives(grid):
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if grid[row][col] > 0:
                grid[row][col] -= 1


import random


def add_apple_at_random_location(grid):
    availbale_pos_apple = [
        (row, col)
        for row in range(len(grid))
        for col in range(len(grid[0]))
        if grid[row][col] == 0
    ]
    if availbale_pos_apple:
        row, col = random.choice(availbale_pos_apple)
        grid[row][col] = -1


def move_snake(app):
    row, col = get_next_head_position(
        app.head_pos, app.direction
    )  # ny retning på hodet ut i fra retning
    new_head_pos = (row, col)
    if not is_legal_move(new_head_pos, app.board):
        app.state = "gameover"
        return
    app.head_pos = new_head_pos
    if app.board[row][col] == -1:
        app.snake_size += 1
        add_apple_at_random_location(app.board)
    else:
        subtract_one_from_all_positives(app.board)
    app.board[row][col] = app.snake_size


def is_legal_move(pos, board):
    row, col = pos
    if not (0 <= row < len(board) and 0 <= col < len(board[0])):
        return False
    if board[row][col] > 0:
        return False
    return True


def timer_fired(app):
    if app.info_mode == False:
        if app.state == "active":
            move_snake(app)


def key_pressed(app, event):
    if event.key == "s":
        app.state = "active"
        app.info_mode = False
    elif event.key == "i":
        app.info_mode = not app.info_mode
    elif (
        app.state == "active" and not app.info_mode
    ):  # her bytter app.info_mode mellom å vre true og false hver gang brukeren trykker på i
        if event.key == "Down":
            app.direction = "south"
        elif event.key == "Up":
            app.direction = "north"
        elif event.key == "Left":
            app.direction = "west"
        elif event.key == "Right":
            app.direction = "east"
        elif event.key == "Space":
            move_snake(app)
        elif app.state == "i":
            app.info_mode = not app.info_mode
    elif event.key == "r":
        app_started(app)
        app.state = "gameover"
        app.state = "welcome"


from snake_view import draw_board


def redraw_all(app, canvas):
    window_height = 400
    window_width = 500
    if app.state == "welcome":
        canvas.create_rectangle(0, 0, window_width, window_height, fill="hotpink")
        canvas.create_text(
            250, 100, text="Welcome to snake", font=("Impact", 40), fill="green"
        )
        canvas.create_text(
            250, 160, text="Press 's' to START", font=("Impact", 28), fill="green"
        )
    elif app.state == "active":
        window_width = 500
        window_height = 400
        x1 = 25
        x2 = window_width - 25
        y1 = (
            25  # margin på 25 hver side            x2 = window_width - 25 #bunntopp -25
        )
        y2 = window_height - 25  # bunnkant -25
        draw_board(canvas, x1, y1, x2, y2, app.board, app.info_mode)
        if app.info_mode:
            canvas.create_text(
                250,
                15,
                text=f"app.head_pos= {app.head_pos}, app.snake_size={app.snake_size}, app.direction={app.direction}, app.state= {app.state}",
            )

    elif app.state == "gameover":
        canvas.create_rectangle(0, 0, window_width, window_height, fill="black")
        canvas.create_text(250, 100, text="GAMEOVER", font=("Impact", 45), fill="red")
        canvas.create_text(
            250, 160, text="Press 'r' to restart", font=("Impact", 28), fill="red"
        )
        canvas.create_text(450, 360, text="DO BETTER", font=("Impact", 12), fill="red")
        canvas.create_text(450, 380, text="BE BETTER", font=("Impact", 12), fill="red")


if __name__ == "__main__":
    from uib_inf100_graphics.event_app import run_app

    run_app(width=500, height=400, title="Snake")


# Hvis graphics ikke funker: trykk Cmd+shift+P, skriv "Python: Select Interpreter:", trykk på 3.12
