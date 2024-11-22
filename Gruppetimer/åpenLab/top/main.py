from game_logic import *
from game_graphics import *
from uib_inf100_graphics.event_app import run_app

def app_started(app):
    initialize_game(app)

def key_pressed(app, event):
    handle_key_press(app, event)

def redraw_all(app, canvas):
    draw_game_board(app, canvas)

run_app(width=400, height=500, title="2048")
