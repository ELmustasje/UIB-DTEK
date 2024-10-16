from snake_view import draw_board
import random
# Denne funksjonen kalles én gang ved programmets oppstart.
# Her skal vi __opprette__ variabler i som behøves i app
def app_started(app):
    app.direction = 'east'
    app.info_mode = True
    app.board =[
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0,-1, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 1, 2, 3, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0, 0],
    ]
    app.snake_size =3
    app.head_pos = (3,4)
    app.state = 'active'
    app.timer_delay= 200
   
# En kontroller.
    # Denne funksjonen kalles ca 10 ganger per sekund som standard.
    # Funksjonen kan __endre på__ eksisterende variabler i app.
def timer_fired(app):
   if not app.info_mode and app.state == 'active':
    move_snake(app)
    

 
# En kontroller.
# Denne funksjonen kalles hver gang brukeren trykker på tastaturet
def key_pressed(app, event):
    
    if event.key == 'i':
        if app.info_mode == True:
            app.info_mode = False
        else:
            app.info_mode = True
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
  

#bergegne ny posisjon til hodet
def ny_hode_pos(head_pos, direction):
    row, col =head_pos

    if direction == 'east':
        col +=1
    elif direction == 'south':
        row +=1
    elif direction == 'west':
        col -=1
    elif direction == 'north':
        row -=1
    return (row,col)

def minus_i_alle_positive_ruter(grid):
    for row in range (len(grid)):
        for col in range(len(grid[0])):  
            if grid[row][col]>0:
                grid[row][col]-=1

def eple_pos(grid):
    tomme_ruter=[]
    for row in range(len(grid)):
        for col in range(len(grid[0])):
            
            if grid[row][col]==0: 
                tomme_ruter.append((row,col))

    if tomme_ruter:
        row, col = random.choice(tomme_ruter)
        grid[row][col] = -1
            
#få slagen til å bevege seg
def move_snake(app):
    ny_head_pos=ny_hode_pos(app.head_pos, app.direction)
    if is_legal_move(ny_head_pos, app.board)== False:
        app.state = 'gameover'
        return
    
    if app.board[ny_head_pos[0]][ny_head_pos[1]]==-1:
        app.snake_size +=1
        eple_pos(app.board)
    else:
        minus_i_alle_positive_ruter(app.board)
    
    app.head_pos=ny_head_pos
    app.board[app.head_pos[0]][app.head_pos[1]]=app.snake_size
    
def is_legal_move(pos, board):
    row, col = pos
    if row < 0 or row >=len(board) or col < 0 or col >=len(board[0]):
        return False
    if board[row][col] > 0:
        return False
    return True
# Visningen.
    # Denne funksjonen tegner vinduet. Funksjonen kalles hver gang
    # modellen har endret seg, eller vinduet har forandret størrelse.
    # Funksjonen kan __lese__ variabler fra app, men har ikke lov til
    # å endre på dem.
def redraw_all(app, canvas):

    if app.state == 'gameover':
        canvas.create_text(250,200, text='Game over', fill= 'pink', font= ('Arial', 75 ))
        return

    if app.info_mode:
        canvas.create_text(
            250,
            10,
            text=f'app.head_pos= {app.head_pos} app.snake_size= {app.snake_size} app.direction={app.direction} app.state={app.state}'
            )
    margin=25
    x1=margin
    y1=margin
    x2= app.width-margin
    y2= app.height-margin
    draw_board(canvas, x1, y1, x2, y2, app.board, app.info_mode)

from uib_inf100_graphics.event_app import run_app
run_app(width=500, height=400, title='Snake')




    


