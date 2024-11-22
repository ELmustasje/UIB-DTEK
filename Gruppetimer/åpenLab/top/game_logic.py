import random

# Starter spillet, ved å tegne brettet osv
def initialize_game(app):
    app.grid = [[0] * 4 for _ in range(4)]
    app.score = 0
    app.game_over = False
    load_high_score(app)  
    add_random_tile(app)
    add_random_tile(app)


# Legger til random tiles etter en tile er flyttet
def add_random_tile(app):
    empty_tiles = [(r, c) for r in range(4) for c in range(4) if app.grid[r][c] == 0]
    if empty_tiles:
        r, c = random.choice(empty_tiles)
        app.grid[r][c] = 2 if random.random() < 0.9 else 4

# Håndterer bruker input
def handle_key_press(app, event):
    if event.key == 'r': 
        initialize_game(app)
        return  

    if app.game_over:
        return

    moved = False
    if event.key == 'Left':
        moved = slide_left(app)
    elif event.key == 'Right':
        moved = slide_right(app)
    elif event.key == 'Up':
        moved = slide_up(app)
    elif event.key == 'Down':
        moved = slide_down(app)

    if moved:
        add_random_tile(app)
        if check_game_over(app):
            app.game_over = True
            if app.score > app.high_score:
                app.high_score = app.score
                save_high_score(app)  


# Sjekker om spillet er tapt
def check_game_over(app):
    # Sjekk om det finnes noen tomme brikker
    for r in range(4):
        for c in range(4):
            if app.grid[r][c] == 0:
                return False

    # Sjekk om noen brikker kan kombineres horisontalt eller vertikalt
    for r in range(4):
        for c in range(4):
            if (c < 3 and app.grid[r][c] == app.grid[r][c + 1]) or \
               (r < 3 and app.grid[r][c] == app.grid[r + 1][c]):
                return False

    # Hvis ingen tomme brikker eller mulige kombinasjoner, er spillet over
    return True



#### FUNKSJONER FOR Å BEVEGE TILES ####
# Gadd ikke skrive kommentarer for hver av bevegelses funksjonene fordi de fungere på omtrent samme måte, skrev derfor detaljert på en istedenfor

# Opp
def slide_up(app):
    moved = False  # Spor om det ble gjort en bevegelse
    for c in range(4):  # Iterer gjennom hver kolonne
        # Lag en liste med alle ikkenull verdier i kolonnen
        non_zero = [app.grid[r][c] for r in range(4) if app.grid[r][c] != 0]
        new_col = []  # Ny liste for den oppdaterte kolonnen
        skip = False  # Brukes for å hoppe over elementer som allerede er kombinert

        # Kombiner verdier i kolonnen fra topp til bunn
        for i in range(len(non_zero)):
            if skip:  # Hopp over hvis forrige element ble kombinert
                skip = False
                continue
            # Sjekk om to påfølgende verdier er like
            if i < len(non_zero) - 1 and non_zero[i] == non_zero[i + 1]:
                new_col.append(non_zero[i] * 2)  # Kombiner verdiene
                app.score += non_zero[i] * 2  # Oppdater poengsummen
                skip = True  # Marker for å hoppe over neste verdi
            else:
                new_col.append(non_zero[i])  # Legg til verdien som den er

        # Fyll ut resten av kolonnen med nuller 
        new_col += [0] * (4 - len(new_col))
        
        # Oppdater kolonnen i grid'en og sjekk om det ble gjort en bevegelse
        for r in range(4):
            if app.grid[r][c] != new_col[r]:
                moved = True
            app.grid[r][c] = new_col[r]
    return moved 

# Ned
def slide_down(app):
    moved = False  
    for c in range(4):  
        non_zero = [app.grid[r][c] for r in range(4) if app.grid[r][c] != 0]
        new_col = [] 
        skip = False  

        for i in range(len(non_zero) - 1, -1, -1):
            if skip:
                skip = False
                continue
         
            if i > 0 and non_zero[i] == non_zero[i - 1]:
                new_col.insert(0, non_zero[i] * 2)  
                app.score += non_zero[i] * 2
                skip = True
            else:
                new_col.insert(0, non_zero[i])  

        new_col = [0] * (4 - len(new_col)) + new_col
        
        for r in range(4):
            if app.grid[r][c] != new_col[r]:
                moved = True
            app.grid[r][c] = new_col[r]
    return moved

# Høyre
def slide_right(app):
    moved = False  
    for row in app.grid:  
    
        non_zero = [num for num in row if num != 0]
        new_row = []  
        skip = False  

        for i in range(len(non_zero) - 1, -1, -1):
            if skip:
                skip = False
                continue
          
            if i > 0 and non_zero[i] == non_zero[i - 1]:
                new_row.insert(0, non_zero[i] * 2) 
                app.score += non_zero[i] * 2  
                skip = True
            else:
                new_row.insert(0, non_zero[i])  

        new_row = [0] * (4 - len(new_row)) + new_row
        
        if row != new_row:
            moved = True
        row[:] = new_row
    return moved

# Venstre
def slide_left(app):
    moved = False  
    for row in app.grid:  
        non_zero = [num for num in row if num != 0]
        new_row = []  
        skip = False  

        for i in range(len(non_zero)):
            if skip:
                skip = False
                continue
       
            if i < len(non_zero) - 1 and non_zero[i] == non_zero[i + 1]:
                new_row.append(non_zero[i] * 2)  
                app.score += non_zero[i] * 2  
                skip = True
            else:
                new_row.append(non_zero[i])  
        new_row += [0] * (4 - len(new_row))
        
        if row != new_row:
            moved = True
        row[:] = new_row
    return moved



#### Håndtering av highscores ####
def save_high_score(app):
    with open("high_score.txt", "w") as file:
        file.write(str(app.high_score))


def load_high_score(app):
    try:
        with open("high_score.txt", "r") as file:
            app.high_score = int(file.read())
    except FileNotFoundError:
        app.high_score = 0 
