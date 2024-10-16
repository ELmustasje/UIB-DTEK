import zipfile
import requests
from io import BytesIO
from PIL import Image, ImageTk

# Globale variabler for bilder
apple_image = None
head_up_image = None
head_down_image = None
head_left_image = None
head_right_image = None
body_bottomleft_image = None
body_bottomright_image = None
body_topleft_image = None
body_topright_image = None
body_horizontal_image = None
body_vertical_image = None
tail_right_image = None
tail_left_image = None
tail_up_image = None
tail_down_image = None

def calculate_cell_size(rows, cols, canvas_width, canvas_height):
    """Beregner cellestørrelse basert på gridstørrelse."""
    cell_width = canvas_width / cols
    cell_height = canvas_height / rows
    return cell_width, cell_height

def load_images(rows, cols):
    """Laster inn alle nødvendige bilder og tilpasser dem til cellestørrelsen."""
    global apple_image, head_up_image, head_down_image, head_left_image, head_right_image
    global body_bottomleft_image, body_bottomright_image, body_topleft_image, body_topright_image
    global body_horizontal_image, body_vertical_image, tail_right_image, tail_left_image, tail_up_image, tail_down_image

    # Laster ned ZIP-fil - Creds for game art pngs: laget av "Clear_code" på OpenGameArt.Org
    url = 'https://opengameart.org/sites/default/files/snake_graphics.zip'
    response = requests.get(url)
    zip_file = zipfile.ZipFile(BytesIO(response.content))
    
    # Beregner hvor stor rutene er
    cell_width, cell_height = calculate_cell_size(rows, cols, 500, 400)

    # Funksjon for å laste inn bilder
    def load_image(file_path, cell_width, cell_height):
        with zip_file.open(file_path) as file:
            pil_image = Image.open(file)
            pil_image = pil_image.resize((int(cell_width), int(cell_height)), Image.Resampling.LANCZOS)
            return ImageTk.PhotoImage(pil_image)
    
    # Laster inn bilder
    apple_image = load_image('Graphics/apple.png', cell_width, cell_height)
    head_up_image = load_image('Graphics/head_up.png', cell_width, cell_height)
    head_down_image = load_image('Graphics/head_down.png', cell_width, cell_height)
    head_left_image = load_image('Graphics/head_left.png', cell_width, cell_height)
    head_right_image = load_image('Graphics/head_right.png', cell_width, cell_height)
    tail_right_image = load_image('Graphics/tail_right.png', cell_width, cell_height)
    tail_left_image = load_image('Graphics/tail_left.png', cell_width, cell_height)
    tail_up_image = load_image('Graphics/tail_up.png', cell_width, cell_height)
    tail_down_image = load_image('Graphics/tail_down.png', cell_width, cell_height)
    body_horizontal_image = load_image('Graphics/body_horizontal.png', cell_width, cell_height)
    body_vertical_image = load_image('Graphics/body_vertical.png', cell_width, cell_height)
    body_topleft_image = load_image('Graphics/body_topleft.png', cell_width, cell_height)
    body_topright_image = load_image('Graphics/body_topright.png', cell_width, cell_height)
    body_bottomleft_image = load_image('Graphics/body_bottomleft.png', cell_width, cell_height)
    body_bottomright_image = load_image('Graphics/body_bottomright.png', cell_width, cell_height)

# Retning av snake bilder

def get_adjacent_direction(board, row, col):
    """Returnerer retning på kroppsegment basert på rutene ved siden av"""
    rows, cols = len(board), len(board[0])
    if col > 0 and board[row][col - 1] > 0:
        return 'horizontal'
    if col < cols - 1 and board[row][col + 1] > 0:
        return 'horizontal'
    if row > 0 and board[row - 1][col] > 0:
        return 'vertical'
    if row < rows - 1 and board[row + 1][col] > 0:
        return 'vertical'
    return None

def get_tail_direction(board, row, col):
    """Bestemmer retning på halen basert på rutene ved siden av"""
    rows, cols = len(board), len(board[0])
    if col > 0 and board[row][col - 1] > 0:
        return 'right'
    if col < cols - 1 and board[row][col + 1] > 0:
        return 'left'
    if row > 0 and board[row - 1][col] > 0:
        return 'down'
    if row < rows - 1 and board[row + 1][col] > 0:
        return 'up'
    return None

def is_corner(board, row, col):
    """Sjekker om nåværende rute er et hjørne."""
    
    left = col > 0 and board[row][col - 1] > 0
    right = col < len(board[0]) - 1 and board[row][col + 1] > 0
    top = row > 0 and board[row - 1][col] > 0
    bottom = row < len(board) - 1 and board[row + 1][col] > 0

    return (left and top) or (left and bottom) or (right and top) or (right and bottom)

def draw_board(canvas, x1, y1, x2, y2, board, info_mode, snake_size=None, direction=None):
    """Tegner brettet med slangen, epler og andre elementer."""

    # Finner hvor stor rutene er
    cell_width, cell_height = calculate_cell_size(len(board), len(board[0]), x2 - x1, y2 - y1)

    # Tegner bakgrunn med lysegrønn farge og mørkegrønn kant
    canvas.create_rectangle(x1, y1, x2, y2, fill="#A9DFBF", outline="#006400", width=6)

    # Finner maksimumsverdi og min-verdi for tall over 0 i brettet -- til hode og hale
    max_value = max(max(row) for row in board)
    min_value_candidates = [value for row in board for value in row if value > 0]
    min_value = min(min_value_candidates) if min_value_candidates else None  # Bruk None hvis ingen gyldige kandidater
    head_image = None

    # Finner slangehodets bilde basert på retning
    if direction == 'north':
        head_image = head_up_image
    elif direction == 'south':
        head_image = head_down_image
    elif direction == 'west':
        head_image = head_left_image
    elif direction == 'east':
        head_image = head_right_image

    # Tegner rutene på brettet
    for row in range(len(board)):
        for col in range(len(board[0])):            
            cell_left = x1 + col * cell_width
            cell_top = y1 + row * cell_height
            cell_right = cell_left + cell_width
            cell_bottom = cell_top + cell_height
            
            """Eple"""
            if board[row][col] == -1:  
                canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=apple_image, anchor='center')
            
            elif board[row][col] == max_value and head_image:
                """Hode""" 
                canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=head_image, anchor='center')
            
            elif board[row][col] == min_value:
                """Hale"""                
                tail_direction = get_tail_direction(board, row, col)
                
                """Finner riktig bilde ift. retning"""
                if tail_direction == 'right':
                    canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=tail_right_image, anchor='center')
                elif tail_direction == 'left':
                    canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=tail_left_image, anchor='center')
                elif tail_direction == 'up':
                    canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=tail_up_image, anchor='center')
                elif tail_direction == 'down':
                    canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=tail_down_image, anchor='center')
            
            else:  
                if min_value < board[row][col] < max_value:
                    """Kropp"""
                    
                    if is_corner(board, row, col):
                        """Finner riktig bilde ift. retning"""

                        # Finner rutene rundt - Trenger det til å finne ut om det er et hjørne
                        # F.eks hvis det er en rute til venstre, men ikke til høyre for en rute, så er det et hjørne

                        # Skjekker om det er en rute til venstre
                        left_part = col > 0 and board[row][col - 1] > 0
                        
                        # Skjekker om det er en rute til høyre
                        right_part = col < len(board[0]) - 1 and board[row][col + 1] > 0

                        # Skjekker om det er en rute til over
                        top_part = row > 0 and board[row - 1][col] > 0

                        # Skjekker om det er en rute til under
                        bottom_part = row < len(board) - 1 and board[row + 1][col] > 0

                        # Bruker riktig bilde basert på hvilke kombinasjoner som passer
                        if top_part and right_part:
                            canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=body_topright_image, anchor='center')
                        elif top_part and left_part:
                            canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=body_topleft_image, anchor='center')
                        elif bottom_part and right_part:
                            canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=body_bottomright_image, anchor='center')
                        elif bottom_part and left_part:
                            canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=body_bottomleft_image, anchor='center')
                    else:
                        # Tegner horisontale eller vertikale bilder ab kroppen til slangen
                        adjacent_direction = get_adjacent_direction(board, row, col)
                        
                        if adjacent_direction == 'horizontal':
                            canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=body_horizontal_image, anchor='center')
                        
                        elif adjacent_direction == 'vertical':
                            canvas.create_image(cell_left + cell_width / 2, cell_top + cell_height / 2, image=body_vertical_image, anchor='center')

            # Info-modus
            if info_mode:
                cell_mid_x = (cell_left + cell_right) / 2
                cell_mid_y = (cell_top + cell_bottom) / 2
                text = f'{row},{col}\n{board[row][col]}'
                canvas.create_text(cell_mid_x, cell_mid_y, text=text, anchor="center")

def get_adjacent_direction(board, row, col):
    """Skjekker om det kroppen har deler over eller ved siden av seg for å finne riktig bilde"""
    rows = len(board)
    cols = len(board[0])

    # Rute til venstre
    if col > 0 and board[row][col - 1] > 0:
        return 'horizontal'
    
    # Rute til høyre
    elif col < cols - 1 and board[row][col + 1] > 0:
        return 'horizontal'
    
    # Rute over
    elif row > 0 and board[row - 1][col] > 0:
        return 'vertical'
    
    # Rute under
    elif row < rows - 1 and board[row + 1][col] > 0:
        return 'vertical'
    
    return None  # Ingen slange del ved noen av sidene til ruten - burde ikke skje

def get_tail_direction(board, row, col):
    """Skjekker hvor ruter befinner seg ift halen for å finne riktig bilde av halen"""
    
    rows = len(board)
    cols = len(board[0])

    # Finner hvor ruten ved siden av halen er

    # Rute til venstre
    if col > 0 and board[row][col - 1] > 0: 
        return 'right'
    
    # Rute til høyre
    elif col < cols - 1 and board[row][col + 1] > 0:
        return 'left'
    
    # Rute over
    elif row > 0 and board[row - 1][col] > 0:
        return 'down'
    
    # Rute under
    elif row < rows - 1 and board[row + 1][col] > 0:
        return 'up'
    
    return None  # Ingen slange del ved noen av sidene til halen - burde ikke skje


def is_corner(board, row, col):
    """Skjekker om rute er et hjørne"""
    
    # Rask sjekk om det er slangen vi sjekker
    if board[row][col] <= 0:
        return False

    # Rute til venstre
    left_part = col > 0 and board[row][col - 1] > 0

    # Rute til høyre
    right_part = col < len(board[0]) - 1 and board[row][col + 1] > 0

    # Rute over
    top_part = row > 0 and board[row - 1][col] > 0

    # Rute under
    bottom_part = row < len(board) - 1 and board[row + 1][col] > 0

    # Bruker logikken hvor dersom en rute finnes på ene siden men ikke på andre så er det et hjørne
    # For eksempel om en kropps-rute finnes til høre, men ikke til venstre, eller oppe men ikke nede
    return (left_part and not right_part) or (right_part and not left_part) or \
           (top_part and not bottom_part) or (bottom_part and not top_part)

import random

def draw_snake(canvas, difficulty='normal'):
    """Bilde av slangen i menyen"""
    
    canvas.create_rectangle(0, 0, 500, 400, fill='light blue')
    difficulty = difficulty.lower()

    # Tegner epler i bakgrunnen hvis bruker velger 'survival'
    if difficulty == 'survival':
        for _ in range(50):
            x = random.randint(0, 480)
            y = random.randint(0, 380)
            canvas.create_image(x, y, image=apple_image, anchor='center')


    # Tegner gule lyn i bakgrunnen hvis bruker velger 'speedy'
    if difficulty == 'speedy':
        for _ in range(10):
            x = random.randint(0, 480)
            y = random.randint(0, 380)
            # Lynet
            canvas.create_line(x, y, x + 10, y + 20, x + 20, y + 10, x + 30, y + 30, fill='yellow', width=3)

    """Bilde av slangen i menyen"""
    # Halsen
    canvas.create_rectangle(150, 50, 350, 400, fill='#DDA0DD')
    # Hodet
    canvas.create_polygon((250, 250), (120, 50), (380, 50), fill='purple', outline='black', width=2)  # Snake head
    
    # Øye farge basert på vanskelighetsgrad
    eye_color = 'white'
    if difficulty == 'normal':
        eye_color = 'white'
    elif difficulty == 'speedy':
        eye_color = 'yellow'
    elif difficulty == 'survival':
        eye_color = 'red'

    # Tegner øyne

    # Venstre
    canvas.create_oval(170, 80, 230, 140, fill=eye_color)
    # Høyre
    canvas.create_oval(270, 80, 330, 140, fill=eye_color)
    # Venstre pupill
    canvas.create_oval(200, 100, 210, 110, fill="black")
    # Høyre pupill
    canvas.create_oval(300, 100, 310, 110, fill="black")

    # Tittel
    canvas.create_text(250, 275, text='S N A K E', font=('Showcard Gothic', 35), fill='black')

    # Munn
    canvas.create_line(230, 200, 270, 200, fill='black', width=2)

    # Tungen
    canvas.create_rectangle(240, 200, 260, 230, fill='red')
    # Tegner en trekant for å få 'slange-tunge'
    canvas.create_polygon((240, 230), (260, 230), (250, 215), fill='purple', outline='purple')

    # Bakgrunn farge til vanskelighetsgrad meny
    canvas.create_rectangle(0, 350, 500, 400, fill='green')

    # Piler som indikerer at man kan endre vanskelighetsgrad

    # Venstre
    x1, y1 = 75, 365  # Venstre hjørne av trekant
    x2, y2 = 75, 385  # Høyre hjørne av trekant
    x3, y3 = 60, 375  # Hjørne i midten av trekant
    canvas.create_polygon((x1, y1), (x2, y2), (x3, y3), fill='black', outline='black')

    # Høyre
    x1_r, y1_r = 425, 365
    x2_r, y2_r = 425, 385  
    x3_r, y3_r = 440, 375 
    canvas.create_polygon((x1_r, y1_r), (x2_r, y2_r), (x3_r, y3_r), fill='black', outline='black')

    # Vanskelighetsgrad display
    canvas.create_text(250, 375, text=difficulty.capitalize(), font=('Showcard Gothic', 20), fill='black')