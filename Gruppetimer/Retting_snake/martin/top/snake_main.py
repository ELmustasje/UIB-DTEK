import random  # For å kunne hente frem tilfeldige tall (for eplene på brettet)  

def app_started(app):  
    # Variabler som skal starte når spillet åpnes
    app.direction = 'east'  # Slangens retning starter mot øst
    app.info_mode = True     # Informasjonsmodus starter som aktiv
    app.state = 'active'     # Spilltilstand, 'active' eller 'gameover'
    app.timer_delay = 200    # Sett timerforsinkelse til 200 millisekunder

    #spillbrettet
    app.board = [
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, -1, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 2, 3, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]

    # variabler for slangens størrelse og hodeposisjon
    app.snake_size = 3        # Startstørrelse på slangen
    app.head_pos = (3, 4)     # Startposisjonen til slangehodet (rad, kolonne)
    app.snake_body = [(3, 4), (3, 3), (3, 2)]  # Start kropp

def timer_fired(app):
    # Utfør move_snake hvis info_mode er av og state er aktiv
    if not app.info_mode and app.state == 'active':
        move_snake(app)

def key_pressed(app, event):
    # Håndterer tastetrykk for å endre retningen og info-modus

    # Slå av/på informasjonsmodus når 'i' trykkes
    if event.key == 'i':
        app.info_mode = not app.info_mode

    # Oppdater retning basert på piltastene
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

def move_snake(app):
    # Oppdater hodeposisjonen basert på retningen
    next_position = get_next_head_position(app.head_pos, app.direction) 

    # Sjekk om neste posisjon er lovlig
    if not is_legal_move(next_position, app.board, app.snake_body):
        app.state = 'gameover'
        return

    app.head_pos = next_position
    app.snake_body.insert(0, app.head_pos)  # Legg til hodeposisjonen i kroppen

    # Sjekk om det er et eple på den nye posisjonen
    row, col = app.head_pos
    if app.board[row][col] == -1:  # Eple er tilstede
        app.snake_size += 1          # Øk slangens størrelse
        add_apple_at_random_location(app.board)  # Plasser nytt eple
    else:
        update_board(app)            # Oppdater brettet
        app.snake_body.pop()         # Fjern den siste delen av slangen ER DENNE NØDVENDIG?

    # Sett den nye posisjonen til hodeverdien
    app.board[row][col] = app.snake_size

def get_next_head_position(head_pos, direction):
    # Finn neste posisjon for slangen sitt hode
    row, col = head_pos
    if direction == 'north':
        return (row - 1, col)
    elif direction == 'south':
        return (row + 1, col)
    elif direction == 'west':
        return (row, col - 1)
    elif direction == 'east':
        return (row, col + 1)

def is_legal_move(pos, board, snake_body):
    row, col = pos
    # Sjekk om posisjonen er innenfor brettets rammer
    if row < 0 or row >= len(board) or col < 0 or col >= len(board[0]):
        return False
    # Sjekk om slangen krysser seg selv
    if pos in snake_body:
        return False
    return True

def update_board(app):
    # Oppdater brettet ved å trekke fra 1 for alle positive verdier
    for r in range(len(app.board)):
        for c in range(len(app.board[r])):
            if app.board[r][c] > 0:
                app.board[r][c] -= 1

def add_apple_at_random_location(grid):
    # Finner et tilfeldig sted for å legge til et eple (-1)
    rows = len(grid)
    cols = len(grid[0])
    empty_positions = []

    for r in range(rows):
        for c in range(cols):
            if grid[r][c] == 0:  # Hvis posisjonen er ledig
                empty_positions.append((r, c))

    if empty_positions:  # Hvis det er ledige posisjoner
        r, c = random.choice(empty_positions)
        grid[r][c] = -1  # Legg til et eple

def redraw_all(app, canvas):
    # Visningen
    # Tegner spillbrettet og eventuelt slangeretningen

    # Importerer draw_board fra snake_view
    from snake_view import draw_board

    if app.state == 'active':
        # Tegner brettet
        draw_board(canvas, 25, 25, app.width - 25, app.height - 25, app.board, app.info_mode)

        # Tegner retningen dersom info_mode er True
        if app.info_mode:
            # Oppdater teksten for å inkludere retning, slangens størrelse og hodeposisjon
            canvas.create_text(250, 10, text=f'{app.direction=}, {app.snake_size=}, {app.head_pos=}, {app.state=}')

    elif app.state == 'gameover':
        canvas.create_text(app.width / 2, app.height / 2, text='Game Over', font=('Arial', 24), fill='red')

# Kjører programmet
if __name__ == '__main__':
    from uib_inf100_graphics.event_app import run_app
    run_app(width=500, height=400, title='Snake')


