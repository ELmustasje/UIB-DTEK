import random
import time
from snake_view import draw_board, draw_snake, load_images

# Define available board sizes
BOARD_SIZES = [
    (10, 15),  # Size 1
    (15, 20),  # Size 2
    (20, 30)   # Size 3
]

def app_started(app, is_restart=False, rows=None, cols=None, difficulty=None):
    if not is_restart:
        """Starter med board selection, og endrer denne til valgt størrelse"""
        app.state = 'board_selection'
        app.board_size_index = 0 
        app.rows, app.cols = BOARD_SIZES[app.board_size_index]
    else:
        """Går rett på countdown, og beholder innstillinger, kjører reset_snake_and_apple og stopper resten av funksjonen"""
        app.state = 'countdown'
        app.rows = rows if rows is not None else app.rows
        app.cols = cols if cols is not None else app.cols
        app.difficulty = difficulty if difficulty is not None else app.difficulty
        # Reset the snake and apple positions during restart
        reset_snake_and_apple(app)
        app.start_time = time.time()  # Start timeren på nytt ved restart

        return

    """Initialiserer app variabler"""
    # Snake - direction og size
    app.direction = 'east'
    app.next_direction = None
    app.snake_size = 3
    app.head_pos = (app.rows // 2, app.cols // 2)
    # Board
    app.board_size_index = 0
    app.rows, app.cols = BOARD_SIZES[app.board_size_index]
    app.info_mode = False
    app.board = [[0 for _ in range(app.cols)] for _ in range(app.rows)]
    app.board[app.head_pos[0]][app.head_pos[1]] = app.snake_size
    # Countdown
    app.countdown = 3
    app.difficulty = 'normal'
    app.size_decrease_timer = 0
    app.start_time = 0 
    app.time = 0 
    # Legger til eple etter alt er initialisert
    add_apple_at_random_location(app)

def reset_snake_and_apple(app):
    """Setter standard verdier til board og eple dersom restart"""
    app.board = [[0 for _ in range(app.cols)] for _ in range(app.rows)] 
    app.snake_size = 3

    # Setter hodet i midten
    app.head_pos = (app.rows // 2, app.cols // 2)
    # Setter verdien til hode "cellen" til hvor lang slangen er - for å få riktig verdier på slangen
    app.board[app.head_pos[0]][app.head_pos[1]] = app.snake_size
    
    add_apple_at_random_location(app)

def timer_fired(app):
    if app.state == 'active' and not app.info_mode and not app.state == 'paused': # Kjører spillet kun når vi ønsker
        if app.start_time == 0:
            # Timer til score tracking
            app.start_time = time.time()
        
        move_snake(app) # Beveger snake hele tiden
        
        # Track tiden i survival
        app.time = float(time.time() - app.start_time)
        if app.difficulty == 'survival':
            app.size_decrease_timer += 1

            # Fjerner 1 fra snake size etter 10 ms
            if app.size_decrease_timer >= 10:
                decrease_snake_size(app)
                app.size_decrease_timer = 0

    # Nedtelling før spillet starter
    elif app.state == 'countdown':
        if app.countdown > 0:
            time.sleep(1)
            app.countdown -= 1
        else:
            app.state = 'active'

    # Kontrollerer antall epler på brettet
    if app.difficulty == 'survival':
        ensure_apples_count(app, 5)
    else:
        ensure_apples_count(app, 1)

def decrease_snake_size(app):
    """Fjerner en rute fra snake size"""
    if app.snake_size <= 1:
        app.state = 'gameover'
        app.start_time = 0
    app.snake_size -= 1

def ensure_apples_count(app, target_count):
    """Spawner eller fjerner epler ut ifra hvor mange vi ber om i parameter target_count"""
    # Teller epler
    current_count = count_apples(app.board)
    # Legger til epler hvis det mangler
    while current_count < target_count:
        add_apple_at_random_location(app)
        current_count += 1
    # Fjerner epler hvis det mangler
    while current_count > target_count:
        remove_apple(app)
        current_count -= 1

def remove_apple(app):
    """Fjerner et tilfeldig eple og stopper loopen"""
    board = app.board
    for r in range(len(board)):
        for c in range(len(board[0])):
            if board[r][c] == -1:  # Hvis det er et eple
                board[r][c] = 0  # Setter verdi til 0 - altså ledig rute
                return

def count_apples(board):
    """Teller epler for antall ruter med verdi -1"""
    return sum(row.count(-1) for row in board) 

def key_pressed(app, event):
    if app.state == 'board_selection':
        if event.key == 'Up':
            
            # Substraherer valg indeksen med 1 og starter på nytt hvis det går under 0
            app.board_size_index = (app.board_size_index - 1) % len(BOARD_SIZES)
        elif event.key == 'Down':
            
            # Adderer valg indeksen med 1 og starter på nytt hvis den overgår listen
            app.board_size_index = (app.board_size_index + 1) % len(BOARD_SIZES)
        elif event.key == 'Enter':
            """Setter brett størrelsen ved valg gjort"""
            app.rows, app.cols = BOARD_SIZES[app.board_size_index]
            
            # Initialiserer board på nytt
            app.board = [[0 for _ in range(app.cols)] for _ in range(app.rows)] 
            
            app.state = 'menu'
            load_images(app.rows, app.cols)
        return
    
    # Pause
    if event.key == 'p':
        if app.state == 'paused':
            app.state = 'active'
        else:
            app.state = 'paused'

    difficulty_map = {
        'normal': 0,
        'speedy': 1,
        'survival': 2
    }

    difficulty_levels = ['normal', 'speedy', 'survival']

    if app.state == 'menu':
        if event.key == 'Left':
            # Henter nåværende indeks av vanskelighetsgradene
            current_difficulty_index = difficulty_map[app.difficulty]

            # Substraherer valg indeksen med 1 og starter på nytt hvis det går under 0
            new_index = (current_difficulty_index - 1) % len(difficulty_levels)

            # Oppdaterer vanskelighetsgraden
            app.difficulty = difficulty_levels[new_index]
        elif event.key == 'Right':
            # Henter nåværende indeks av vanskelighetsgradene
            current_difficulty_index = difficulty_map[app.difficulty]
            
            # Adderer valg indeksen med 1 og starter på nytt hvis det går over listen
            new_index = (current_difficulty_index + 1) % len(difficulty_levels)
            
            # Oppdaterer vanskelighetsgraden
            app.difficulty = difficulty_levels[new_index]
        if event.key == 'Enter':
           
            set_difficulty_speed(app)
            app.state = 'countdown'

    if event.key == 'i':
        app.info_mode = not app.info_mode

    direction_map = {
        'Up': 'north',
        'Down': 'south',
        'Left': 'west',
        'Right': 'east'
    }

    opposite_direction = {
        'north': 'south',
        'south': 'north',
        'west': 'east',
        'east': 'west'
    }

    if app.state == 'gameover':
        if event.key == 'r':
            # Restart sender gjennom brett størrelse og vanskelighetsgrad slik at den lagres til neste runde
            app_started(app, is_restart=True, rows=app.rows, cols=app.cols, difficulty=app.difficulty)
        
        elif event.key == 'b':
            # Restarter og sender gjennom is_restart=False slik at vi skiller mellom restart og å begynne helt fra starten
            app_started(app, is_restart=False)
            return

    if app.state == 'gameover':
        return

    if event.key in direction_map:
        """Tar hånd om snake kontroller med piltastene"""
        new_direction = direction_map[event.key]
        
        if app.direction != opposite_direction[new_direction]:
            """Tar hensikt til at man ikke kan drepe seg selv ved å gå i motsatt retning"""
            app.next_direction = new_direction

def draw_intro_screen(canvas, app):
    # Tegn bakgrunnsfirkant med en renere grønnfarge
    canvas.create_rectangle(10, 10, 490, 390, outline='black', fill='#81C784')

    # Tittel
    canvas.create_text(250, 30, text='Welcome to Snake Game', font=('Courier New', 24, 'bold'), fill='#1B5E20')

    # Hvordan spille
    canvas.create_text(250, 70, text='How to Play:', font=('Courier New', 18, 'bold'), fill='#1B5E20')
    canvas.create_text(250, 100, text='Enter: Next Page, Up/Down: Board size', font=('Courier New', 12), fill='black') 
    canvas.create_text(250, 120, text='Arrow Keys: Move the Snake, p: Pause, i: Info', font=('Courier New', 12), fill='black')
    
    # Game over kontrols
    canvas.create_text(250, 150, text='When you die:', font=('Courier New', 18, 'bold'), fill='#1B5E20')
    canvas.create_text(250, 175, text='R: Restart', font=('Courier New', 14), fill='black')
    canvas.create_text(250, 195, text='B: Back to Start', font=('Courier New', 14), fill='black')

    # Deling mellom spillinfo og brettstørrelse
    canvas.create_line(10, 220, 490, 220, fill='#1B5E20')  # Mørk grønn

    # Brettstørrelse
    canvas.create_text(100, 240, text='Size:', font=('Courier New', 18, 'bold'), fill='#1B5E20')  # Mørkere grønn
    for i, (rows, cols) in enumerate(BOARD_SIZES):
        # Vis hvilken brettstørrelse som er valgt
        selection = ">> " if i == app.board_size_index else "   "
        canvas.create_text(70, 270 + i * 30, text=f'{selection} {rows} x {cols}', font=('Courier New', 14), fill='black')  # Mørk grå

    # Spillmoduser
    canvas.create_text(380, 240, text='Modes:', font=('Courier New', 18, 'bold'), fill='#1B5E20')  # Mørkere grønn
    canvas.create_text(380, 270, text='Normal: standard', font=('Courier New', 14), fill='black')  # Mørk grå
    canvas.create_text(380, 290, text='Speedy: faster', font=('Courier New', 14), fill='black')   # Mørk grå
    canvas.create_text(380, 310, text='Survival: Shrinks!', font=('Courier New', 14), fill='black')  # Mørk grå


def redraw_all(app, canvas):
    """Rendrer alt på skjermen basert på nåværende tilstand"""

    if app.state == 'board_selection':
        draw_intro_screen(canvas, app)
        return

    if app.state == 'menu':
        """Viser menyen etter brettvalg, der vanskelighetsgraden velges"""
        draw_snake(canvas, str(app.difficulty))  # Viser slangen sammen med vanskelighetsgraden
        return

    if app.info_mode:
        """Viser informasjon om spillets nåværende tilstand i info mode"""
        canvas.create_text(250, 10, text=f'{app.state=} {app.head_pos=} {app.snake_size=} {app.direction=}')

    if app.state == 'gameover':
        """Viser game over skjerm"""
        canvas.create_text(250, 150, text='Game Over', font='Arial 30 bold')
        canvas.create_text(250, 250, text='Trykk på "r" for å prøve på nytt', font='Arial 10 italic')
        canvas.create_text(250, 300, text='Eller "b" for å gå tilbake', font='Arial 10 italic')

        return

    if app.state == 'countdown':
        """Viser nedtellingsskjerm før spillet starter"""
        countdown_text = "GO!" if app.countdown == 0 else str(app.countdown)
        canvas.create_text(250, 200, text=countdown_text, font='Arial 40 bold')
        return

    # draw_board fra snake_view for å tegne slangen og brettet
    draw_board(canvas, 25, 25, app.width - 25, app.height - 25 - 30, app.board, app.info_mode, app.snake_size, app.direction)
    
    if app.difficulty == 'survival':
        """Viser tid i overlevelsesmodus"""
        canvas.create_text(250, 375, text=f'{app.time:.2f}', font='Arial 25 bold')
    else:
        """Viser poeng, som er slangelengden minus 3 (startlengden)"""
        canvas.create_text(250, 375, text=app.snake_size - 3, font='Arial 30 bold')

    if app.state == 'paused':
        """Viser paused-skjerm hvis spillet er satt på pause"""
        canvas.create_text(250, 200, text='Paused', font='Arial 40 bold')
def set_difficulty_speed(app):
    """Setter hastigheten på spillet basert på valgt vanskelighetsgrad"""
    if app.difficulty == 'normal':
        app.timer_delay = 100  # Normal hastighet
    elif app.difficulty == 'speedy':
        app.timer_delay = 60   # Raskere hastighet
    elif app.difficulty == 'survival':
        app.timer_delay = 100  # Survival-modus bruker normal hastighet, men slangen krymper over tid

def move_snake(app):
    """Beveger slangen i valgt retning, oppdaterer hodets posisjon og sjekker om bevegelsen er lovlig"""
    if app.next_direction:
        app.direction = app.next_direction  # Oppdaterer retning hvis en ny retning er valgt
        app.next_direction = None

    # Beregn den nye hodet posisjonen basert på retning
    app.head_pos = get_next_head_position(app.head_pos, app.direction)
    
    # Sjekk om bevegelsen er lovlig (innenfor brettet og ikke på slangen selv)
    check_move = is_legal_move(app.head_pos, app.board)

    if not check_move:
        app.state = 'gameover'  # Hvis bevegelsen ikke er lovlig, sett spillet til game over
        return

    row, col = app.head_pos
    if app.board[row][col] == -1:  # Hvis slangen spiser et eple
        app.snake_size += 1
        
        # Legg til et nytt eple i normal- og speedy-modus når et er spist
        if app.difficulty != 'survival':
            add_apple_at_random_location(app)

    # Oppdater brettet for å reflektere slangens bevegelse
    update_board(app.board)
    app.board[row][col] = app.snake_size  # Sett hodets nye posisjon

def get_next_head_position(head_pos, direction):
    """Returnerer den nye posisjonen til slangens hode basert på retning"""
    direction_move = {
        'north': (-1, 0),
        'south': (1, 0),
        'west': (0, -1),
        'east': (0, 1)
    }
    move = direction_move.get(direction, (0, 0))
    return (head_pos[0] + move[0], head_pos[1] + move[1])

def is_legal_move(head_pos, board):
    """Sjekker om den nye posisjonen til hodet er innenfor brettet og om den er ledig"""
    row, col = head_pos
    if 0 <= row < len(board) and 0 <= col < len(board[0]) and (board[row][col] == 0 or board[row][col] == -1):
        return True  # Bevegelsen er lovlig
    return False

def add_apple_at_random_location(app):
    """Plasserer et eple tilfeldig på brettet"""
    empty_cells = [(r, c) for r in range(app.rows) for c in range(app.cols) if app.board[r][c] == 0]
    if empty_cells:
        r, c = random.choice(empty_cells)
        app.board[r][c] = -1  # Plasser eplet

def update_board(board):
    """Oppdaterer brettet ved å redusere slangens kroppsdeler med 1 for hver runde"""
    for r in range(len(board)):
        for c in range(len(board[0])):
            if board[r][c] > 0:
                board[r][c] -= 1  # Kroppsdelen blir eldre, reduserer verdien

if __name__ == '__main__':
    from uib_inf100_graphics.event_app import run_app
    run_app(width=500, height=400, title='Snake')  # Starter spillet
