import math

def get_tile_color(value):
    if value == 0:
        return "lightgrey"
    else:
        # Start med "orange" og gjør fargen mørkere basert på verdien
        base_color = [255, 165, 0]  
        max_value = 2048
        scale = min(value, max_value)
        
        # Bruk logaritmisk skala for å beregne en mørkere intensitet
        factor = math.log(scale, 2) / math.log(max_value, 2)
        darker_color = [
            int(base_color[i] * (1 - 0.8 * factor))  
            for i in range(3)
        ]
        
        # Konverter RGB til hex format
        return f"#{darker_color[0]:02x}{darker_color[1]:02x}{darker_color[2]:02x}"

def draw_game_board(app, canvas):
    # Tegner tilsene
    for r in range(4):
        for c in range(4):
            value = app.grid[r][c]
            x0, y0, x1, y1 = c * 100, r * 100, (c + 1) * 100, (r + 1) * 100
            color = get_tile_color(value)
            canvas.create_rectangle(x0, y0, x1, y1, fill=color, outline="black")
            if value != 0:
                canvas.create_text((x0 + x1) / 2, (y0 + y1) / 2, text=str(value),
                                   font="Helvetica 24 bold", fill="white")

    # Viser score og highscore 
    canvas.create_text(200, 420, text=f"Score: {app.score}", font="Helvetica 16 bold")
    canvas.create_text(200, 450, text=f"High Score: {app.high_score}", font="Helvetica 16 bold")

    # Game over melding som blir tegnet om spillern taper
    if app.game_over:
        canvas.create_rectangle(50, 150, 350, 300, fill="white", outline="red", width=2)
        canvas.create_text(200, 200, text="Game Over!", font="Helvetica 32 bold", fill="red")
        canvas.create_text(200, 250, text="Press 'R' to restart!", font="Helvetica 16", fill="grey")