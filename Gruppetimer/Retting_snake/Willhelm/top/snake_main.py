def app_started(app):
    app.direction = "east"
    app.info_mode = True
    app.snake_size = 3
    app.head_pos = (3, 4)
    app.state = "start"
    app.board = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, -1, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 2, 3, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]
    app.timer_delay = 200

    # Modellen.
    # Denne funksjonen kalles én gang ved programmets oppstart.
    # Her skal vi __opprette__ variabler i som behøves i app.
    ...


def timer_fired(app):
    if not app.info_mode and app.state == "active":
        move_snake(app)
    # En kontroller.
    # Denne funksjonen kalles ca 10 ganger per sekund som standard.
    # Funksjonen kan __endre på__ eksisterende variabler i app.
    ...


def key_pressed(app, event):
    if app.state in ["start", "gameover"] and event.key == "Enter":
        app_started(app)
        app.state = "active"
        app.info_mode = False
    elif app.state == "active":
        if event.key == "p":
            app.state = "paused"
        elif event.key == "i":
            app.info_mode = not app.info_mode
        elif event.key == "Up":
            app.direction = "north"
        elif event.key == "Down":
            app.direction = "south"
        elif event.key == "Left":
            app.direction = "west"
        elif event.key == "Right":
            app.direction = "east"
        elif event.key == "Space":
            move_snake(app)
    elif app.state == "paused" and event.key == "p":
        app.state = "active"
    elif event.key == "i":
        app.info_mode = not app.info_mode

    # En kontroller.
    # Denne funksjonen kalles hver gang brukeren trykker på tastaturet.
    # Funksjonen kan __endre på__ eksisterende variabler i app.
    ...


from snake_view import draw_board


def redraw_all(app, canvas):
    if app.state == "start":
        canvas.create_text(
            app.width / 2,
            app.height / 2 - 20,
            text="Snake Game",
            font="Arial 30 bold",
            fill="green",
        )
        canvas.create_text(
            app.width / 2,
            app.height / 2 + 20,
            text="Press Enter to Start",
            font="Arial 20",
            fill="blue",
        )
    elif app.state == "active":
        draw_board(
            canvas, 25, 25, app.width - 25, app.height - 25, app.board, app.info_mode
        )
        if app.info_mode:
            canvas.create_text(
                250,
                12,
                text=f"Direction: {app.direction} Snake size: {app.snake_size} Head position: {app.head_pos}, State: {app.state}",
                font="Arial 16 bold",
            )
    else:
        canvas.create_text(
            app.width / 2,
            app.height / 2,
            text="Game over",
            font="arial 30 bold",
            fill="red",
        )
        canvas.create_text(
            app.width / 2,
            app.height / 2 + 30,
            text="Press Enter to try again",
            font="Arial 20",
            fill="blue",
        )
    # canvas.create_text(250, 12, text=f"Snake size: {app.snake_size}", font="Arial 16")
    # canvas.create_text(250, 12, text=f"Head position: {app.head_pos}", font="Arial 16")

    # Visningen.
    # Denne funksjonen tegner vinduet. Funksjonen kalles hver gang
    # modellen har endret seg, eller vinduet har forandret størrelse.
    # Funksjonen kan __lese__ variabler fra app, men har ikke lov til
    # å endre på dem.
    ...


[
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, -1, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 1, 2, 3, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0],
]


def get_next_head_position(head_pos, direction):
    row, col = head_pos
    if direction == "north":
        return (row - 1, col)
    elif direction == "south":
        return (row + 1, col)
    elif direction == "west":
        return (row, col - 1)
    elif direction == "east":
        return (row, col + 1)


def subtract_one_from_all_positives(grid):
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            if grid[row][col] > 0:
                grid[row][col] -= 1


def move_snake(app):
    new_head_pos = get_next_head_position(app.head_pos, app.direction)

    if not is_legal_move(new_head_pos, app.board):
        app.state = "gameover"
        return

    row, col = new_head_pos
    if app.board[row][col] == -1:
        app.snake_size += 1
        add_apple_at_random_location(app.board)
    else:
        subtract_one_from_all_positives(app.board)

    app.head_pos = new_head_pos
    app.board[row][col] = app.snake_size


import random


def add_apple_at_random_location(grid):
    rows = len(grid)
    cols = len(grid[0])
    if rows == 0 or cols == 0:
        raise ValueError("Gridet må ha minst en rad og en kolonne")
    while True:
        row = random.randint(0, rows - 1)
        col = random.randint(0, cols - 1)

        if grid[row][col] == 0:
            grid[row][col] = -1
            break


def is_legal_move(pos, board):
    row, col = pos
    rows = len(board)
    cols = len(board[0])

    if row < 0 or row >= rows or col < 0 or col >= cols:
        return False

    if board[row][col] > 0:
        return False

    return True


if __name__ == "__main__":
    from uib_inf100_graphics.event_app import run_app

    run_app(width=500, height=400, title="Snake")
