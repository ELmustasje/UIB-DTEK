from snake_view import draw_board
import random 

def app_started(app):
    # Modellen.
    # Denne funksjonen kalles én gang ved programmets oppstart.
    # Her skal vi __opprette__ variabler i som behøves i app.
    app.timer_delay = 200
    app.speed_mode = 'Slow'
    app.board_mode = 'Small'
    app.pause_mode = True

    app.direction = 'east'
    app.info_mode = False
    app.state = 'active'
    app.meny = True
    app.highscore = False
    app.writing = False
    
    app.score = 0
    app.apples_picked = 0
    app.score_multi = 1
    app.score_speed = 1
    app.score_board = 3
    app.high_score_value = 0

    app.navn = ''
    app.navn_bank = []
    app.navn_spiller = ''

    app.snake_size = 3
    app.head_pos = (3, 4)

    app.board =[
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0,-1, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 1, 2, 3, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            ]


def timer_fired(app):
    # En kontroller.
    # Denne funksjonen kalles ca 10 ganger per sekund som standard.
    # Funksjonen kan __endre på__ eksisterende variabler i app.
    if app.meny == False and app.state == 'active' and app.info_mode == False and app.pause_mode == False:
        move_snake(app)


def move_snake(app):
    #Funksjon for flytting av slange
    #Oppdatert posisjon for hodet
    app.head_pos = get_next_head_position(app.head_pos, app.direction)
    row, col = app.head_pos

    if is_legal_move(app.head_pos, app.board) == False:
        app.state = 'gameover'

        #Resetter alle verdiene for ett nytt spill
        app.snake_size = 3
        app.head_pos = (3, 4)
        app.direction = 'east'
        app.pause_mode = True

        if app.board_mode == 'Small':
            app.board =[
                [0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0,-1, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 2, 3, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0],
                ]
        elif app.board_mode == 'Bigger':
            app.board = [
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 2, 3, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                ]
        elif app.board_mode == 'Massive':
            app.board = [
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
                ]
        return
    
    #Sjekker om det er eple eller ikke
    if app.board[row][col] == -1:
        app.snake_size += 1
        #Multien regnes ut
        app.score_multi = app.score_speed * app.score_board
        app.score += app.score_multi
        app.apples_picked += 1
        if app.score > app.high_score_value:
                app.high_score_value = app.score
        app.board[row][col] = app.snake_size
        app.board = add_apple_at_random_location(app.board)
    else:
        app.board = subtract_one_from_all_positives(app.board)
        app.board[row][col] = app.snake_size


def get_next_head_position(head_pos, direction):
    #Hjelpe funksjon for hodeposisjon til slangen
    (x, y) = head_pos

    if direction == 'north':
        x = x - 1
    if direction == 'south':
        x = x + 1
    if direction == 'west':
        y = y - 1
    if direction == 'east':
        y = y + 1
    return((x, y))

def name_on_player(bank):
    mengde_navn = len(bank)

    for i in range(mengde_navn):
        navn = bank[i]
    
    return(navn)
    
def add_apple_at_random_location(grid):
    #Legger til ett nytt tilfeldigt eple
    rows = len(grid)
    cols = len(grid[0])

    #Sjekker 10 posisjoner etter en rask ledig plass
    for i in range(10):
        rand_row = random.choice(range(rows))
        rand_col = random.choice(range(cols))
        if grid[rand_row][rand_col] == 0:
            grid[rand_row][rand_col] = -1
            return(grid)

    #Hvis det ikke kom noen ledig plass sjekker man hvilke som er ledig
    ledige_pos = []

    for rows in range(rows):
        for cols in range(cols):
            if grid[rows][cols] == 0:
                ledige_pos.append([rows, cols])

    row_ledige_pos = len(ledige_pos)
    tilfeldig_pos = random.choice(range(row_ledige_pos))
    (x, y) = ledige_pos[tilfeldig_pos]
    grid[x][y] = -1
    return(grid)


def subtract_one_from_all_positives(grid):
    #Hjelpefunksjon for moving snake
    rows = len(grid)
    cols = len(grid[0])

    for row in range(rows):
        for col in range(cols):
            if grid[row][col] > 0:
                grid[row][col] = grid[row][col] - 1
                
    return(grid)


def is_legal_move(pos, board):
    #Sjekker om slangen er innfor kartet
    (x, y) = pos

    rows = len(board)
    cols = len(board[0])

    if x < 0 or y < 0:
        return(False)
    elif x >= rows or y >= cols:
        return(False)
    elif board[x][y] > 0:
        return(False)
    else:
        return(True)


def key_pressed(app, event):
    # En kontroller.
    # Denne funksjonen kalles hver gang brukeren trykker på tastaturet.
    # Funksjonen kan __endre på__ eksisterende variabler i app.

    #Start Meny knapper
    if app.meny == True:
        #For og starte spillet
        if event.key == 's':
            app.meny = False
        #For hastighets nivå
        elif event.key == '1':
            app.timer_delay = 200
            app.speed_mode = 'Slow'
            app.score_speed = 1
        elif event.key == '2':
            app.timer_delay = 128
            app.speed_mode = 'Fast'
            app.score_speed = 2
        elif event.key == '3':
            app.timer_delay = 50
            app.speed_mode = 'Ultra Speed'
            app.score_speed = 4
        #For board størrelse
        elif event.key == 'a':
            app.board =[
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0,-1, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 1, 2, 3, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0],
            ]
            app.board_mode = 'Small'
            app.score_board = 3
        elif event.key == 'b':
            app.board = [
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 1, 2, 3, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            ]
            app.board_mode = 'Bigger'
            app.score_board = 2
        elif event.key == 'c':
            app.board = [
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0,-1, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            ]
            app.board_mode = 'Massive'
            app.score_board = 1   
            

    #Tilbake til meny fra Game Over side
    if app.meny == False and app.state == 'gameover':
        if event.key == 'm' and app.highscore == False:
            app.meny = True
            app.state = 'active'
            app.score = 0
            app.apples_picked = 0
            app.direction = 'east'
        if event.key == 'r' and app.highscore == False:
            app.state = 'active'
            app.score = 0
            app.apples_picked = 0
            app.direction = 'east'
        if event.key == 'h':
            if app.highscore == True:
                app.highscore = False
            else:
                app.highscore = True

    #Highscore tavle
    if app.highscore == True:
        if event.key in ['Enter', 'Return'] and app.score >= app.high_score_value:
            if app.writing == True:
                app.writing = False
                app.navn_bank.append(app.navn)
                app.navn = ''
                app.navn_spiller = name_on_player(app.navn_bank)
            else:
                app.writing = True
    
    #Skriving
    if app.writing == True and app.highscore == True:
        if event.key == 'Space':
            app.navn += ' '
        elif event.key in ['Enter', 'Return']:
            app.navn += ''  
        elif event.key.lower() == 'backspace':
            if len(app.navn) > 0:
                app.navn = app.navn[:-1]
        else:
            app.navn += event.key

    


    #Info mode av og på
    if event.key == 'i' and app.meny == False and app.state == 'active':
        if app.info_mode == True:
            app.info_mode = False
        else:
            app.info_mode = True
    
    #Sakte modus
    if app.info_mode == True:
        if event.key == 'f':
            move_snake(app)

    #Piltast styring
    if app.meny == False:
        if event.key == 'Up' and app.direction != 'south' and app.pause_mode == False:
            app.direction = 'north'
        elif event.key == 'Down' and app.direction != 'north' and app.pause_mode == False:
            app.direction = 'south'
        elif event.key == 'Left' and app.direction != 'east' and app.pause_mode == False:
            app.direction = 'west'
        elif event.key == 'Right' and app.direction != 'west' and app.pause_mode == False:
            app.direction = 'east'
        elif event.key == 'Space':
            if app.pause_mode == True:
                app.pause_mode = False
            else:
                app.pause_mode = True

    


def redraw_all(app, canvas):
    # Visningen.
    # Denne funksjonen tegner vinduet. Funksjonen kalles hver gang
    # modellen har endret seg, eller vinduet har forandret størrelse.
    # Funksjonen kan __lese__ variabler fra app, men har ikke lov til
    # å endre på dem.

    #Meny tekst
    hoved_meny = f'''\
        - To start the game press (s)
        - To pause the game press (Space)
        - Arrow directions (Up, Down, Left, Right)
        - For info mode press (i) 
          while in info mode to move press (f)'''


    #Tegner hoved menyen
    if app.meny == True:
        canvas.create_rectangle(0, 0, app.width, app.height, fill='#D0FF14')
        canvas.create_text(app.width/2, 10, text='SNAKE', anchor="n", font=('Times new roman', 24),fill='#006B3C')
        canvas.create_text(app.width/2, 50, text='General info:', anchor="n", font=('Times new roman', 16),)
        canvas.create_text(app.width/2, 75, text=hoved_meny, anchor="n", font=('Times new roman', 12),)
        canvas.create_text(app.width/2, 200,
                           text='Different Game Modes: Speed and Map',
                           anchor="n", font=('Times new roman', 16),)
        canvas.create_text(app.width/2, 225,
                           text='Press a NUMBER for speed and a LETTER for map!',
                           anchor="n", font=('Times new roman', 14),)
        canvas.create_text(app.width/2, 250,
                           text=f'Din mode: {app.speed_mode} and {app.board_mode}\t\tScore Multi: {app.score_speed * app.score_board}',
                           anchor="n", font=('Times new roman', 12),)
        canvas.create_text(app.width/2, 275,
                           text='SPEED:\t\t\t\tMAP:\n(1) Slow\t\t\t\t(a) Small\n(2) Fast\t\t\t\t(b) Bigger\n(3) Ultra Speed\t\t\t(c) Massive',
                           anchor="n", font=('Times new roman', 12),)
        

    #Info modus
    if app.info_mode == True and app.state == 'active':
        canvas.create_text(
        15, 5,
        anchor='nw', text=f'{app.direction=}, {app.snake_size=}, {app.head_pos=}, {app.state=}', font='Arial 10',)


    #Spille kjører hvis regler overholdes
    #Her kjøres selve spillbrettet
    if app.state == 'active' and app.meny == False:
        draw_board(canvas, 25, 25, app.width - 25, app.height - 25, app.board, app.info_mode)
        canvas.create_text(app.width/2-100, app.height-10, text = f'Your score: {app.score}', font=('Times new roman', 12, ''), fill='Black')
        canvas.create_text(app.width/2+100, app.height-10, text = f'Highscore: {app.high_score_value}', font=('Times new roman', 12, ''), fill='Black')
        #Hvis pause mode er sant så kommer det opp en pause skjerm
        if app.pause_mode == True:
            canvas.create_rectangle(25, 25, app.width-25, app.height-25, fill='#D0FF14')
            canvas.create_text(app.width/2, app.height/2, text = 'Your Game is Paused', font=('Times new roman', 24, ''), fill='Black')
            
        #Her kjøres Slutt skjermen
    elif app.state == 'gameover' and app.meny == False:
        canvas.create_rectangle(0, 0, app.width, app.height, fill='#006B3C')
        canvas.create_text(app.width/2, 70, text = 'Game Over', font=('Times new roman', 70, ''), fill='Red')
        canvas.create_text(app.width/2, 200,
                        text = f'Your score: {app.score} and Score multi: {app.score_speed * app.score_board}\nAppels picked up: {app.apples_picked}\nYour mode: {app.speed_mode} and {app.board_mode}\n', 
                        font=('Times new roman', 16, ''), fill='White')
        canvas.create_text(app.width/2, 300,
                        text = f'\n\nBack to main menu press:(m)\nRestart game press: (r)\nTo se the HighScore press: (h)\n', 
                        font=('Times new roman', 16, ''), fill='White')
        if app.score >= app.high_score_value:
            canvas.create_text(app.width/2, 130,
                        text = f'NEW HIGHSCORE {app.high_score_value}!', 
                        font=('Times new roman', 16, ''), fill='Purple')
        if app.highscore == True:
            canvas.create_rectangle(25, 25, app.width-25, app.height-25, fill='#D0FF14')
            canvas.create_text(app.width/2, 75, text='HIGHSCORE', font=('Times new roman',24), fill = 'Black')
            canvas.create_text(app.width/2, 100, text='For og skrive ditt navn trykk på (Enter)\nAvslutt skriving ved og trykke (Enter)', font=('Times new roman',10), fill = 'Black')
            canvas.create_text(app.width/2, 130, text ='ONLY ONE NAME ON THE BOARD', font=('Times new roman', 16, ''), fill='Purple')
            canvas.create_text(app.width/2, 250, text=f'{app.navn_spiller}{app.navn}\t\tScore: [{app.high_score_value}]', font=('Times new roman',16), fill = 'Black')



#####################################
#Tester hjelpe funksjon
#####################################


if __name__ == '__main__':
    from uib_inf100_graphics.event_app import run_app
    run_app(width=500, height=400, title='Snake')
