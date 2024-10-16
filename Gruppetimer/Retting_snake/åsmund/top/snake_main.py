from snake_view import draw_board
import random

"""
base score: 20.
ekstra ting:
    
"""


def app_started(app):
    # Modellen.
    # Denne funksjonen kalles én gang ved programmets oppstart.
    # Her skal vi __opprette__ variabler i som behøves i app.
    app.retningstaster = {
        "Up": "north",
        "Down": "south",
        "Left": "west",
        "Right": "east",
    }
    app.motsatt_himmelretning = {
        "south": "north",
        "north": "south",
        "east": "west",
        "west": "east",
    }
    app.retningsverdi = {
        "north": (-1, 0),
        "south": (1, 0),
        "west": (0, -1),
        "east": (0, 1),
    }
    app.difficulty_modes = {"e": "Easy", "h": "Hard"}
    app.info_mode = False
    app.state = "startscreen"
    app.difficulty = "Easy"

    forberede_score()
    hente_scores(app)
    resett_brett(app)


def forberede_score():
    # hentet fra: https://inf100.ii.uib.no/notat/filer/
    import os

    directory_of_current_file = os.path.dirname(__file__)
    os.chdir(directory_of_current_file)  # endrer cwd


def hente_scores(app):
    from pathlib import Path

    app.Path = Path
    try:
        csv_streng = Path("High_scores.csv").read_text(encoding="utf-8")
    except:
        csv_streng = ""
    csv_streng = csv_streng.strip()
    csv_liste = csv_streng.split("\n")
    csv_liste.pop(0)
    app.scores = []
    for l in csv_liste:
        linje = l.split(",")
        app.scores.append((int(linje[0]), linje[1]))
    app.scores.sort()


def lagre_ny_score(app):
    app.scores.append((int(app.score), app.name))
    lagre_scores(app)


def lagre_scores(app):
    app.scores.sort()
    csv_streng = "Navn,Score"
    for r in range(1, len(app.scores) + 1):
        csv_streng += "\n" + str(app.scores[-r][0]) + "," + str(app.scores[-r][1])
    app.Path("High_scores.csv").write_text(csv_streng, encoding="utf-8")


def resett_brett(app):
    if app.difficulty == "Hard":
        app.direction = "east"
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
    elif app.difficulty == "Easy":
        app.direction = "south"
        app.board = [
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 1, 2, 3, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
            [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        ]
        app.snake_size = 3
        app.head_pos = (3, 4)

    app.timer_delay = 900  # milliseconds
    app.direction_queue = []
    app.score = 0
    app.frames = 0
    app.name = ""


def timer_fired(app):
    # En kontroller.
    # Denne funksjonen kalles ca 10 ganger per sekund som standard.
    # Funksjonen kan __endre på__ eksisterende variabler i app.
    if not (app.info_mode or not (app.state == "active")):
        move_snake(app)
        app.score += app.snake_size
        app.frames += 1
        app.timer_delay = int((200 / (1 + app.frames / 50)) + 80)


def key_pressed(app, event):
    # En kontroller.
    # Denne funksjonen kalles hver gang brukeren trykker på tastaturet.
    # Funksjonen kan __endre på__ eksisterende variabler i app.

    if event.key == "i" and not (app.state == "scoreboard"):
        app.info_mode = not (app.info_mode)
        app.score = 0
    if app.state == "active":
        if event.key == "Space" and app.info_mode:
            move_snake(app)

        if event.key in app.retningstaster:
            app.direction = app.retningstaster[event.key]  # standard retning
            # app.direction_queue.append(app.retningstaster[event.key])  # retnings-kø

    elif app.state == "gameover":
        if event.key == "Space" or (event.key == "Enter" or event.key == "Return"):
            if app.difficulty == "Hard":
                app.state = "write_name"
            else:
                app.state = "startscreen"
    elif app.state == "write_name":
        if (event.key == "Backspace" or event.key == "BackSpace") and app.name:
            app.name = app.name[:-1]
        elif event.key == "Enter" or event.key == "Return":
            if app.name:
                lagre_ny_score(app)
            app.state = "scoreboard"
        elif event.key == "Space":
            app.name += " "
        else:
            app.name += str(event.key)
    elif app.state == "scoreboard":
        if (
            event.key == "Space"
            or (event.key == "Enter" or event.key == "Return")
            or (event.key == "Backspace" or event.key == "BackSpace")
        ):
            app.state = "startscreen"
    elif app.state == "startscreen":
        if event.key == "Space" or (event.key == "Enter" or event.key == "Return"):
            app.state = "active"
            resett_brett(app)
        if event.key in app.difficulty_modes:
            app.difficulty = app.difficulty_modes[event.key]


def move_snake(app):
    # app.head_pos = move_head(app.head_pos,app.direction,app.retningsverdi) #standard

    app.head_pos = move_head_queue(app)  # med retnings-kø

    if not (is_legal_move(app.head_pos, app.board)):
        app.state = "gameover"
        return

    if (
        0 <= app.board[app.head_pos[0]][app.head_pos[1]] <= 1
    ):  # forhindrer bug der den tror at halen er mat
        app.board = subtract_one_from_all_positives(app.board)
    else:
        app.snake_size += 1
        add_apple_at_random_location(app.board)

    app.board[app.head_pos[0]][app.head_pos[1]] = app.snake_size


def move_head(posisjon, direction, retningsverdi):  # standard
    retningstuple = retningsverdi[direction]
    return (posisjon[0] + retningstuple[0], posisjon[1] + retningstuple[1])


def move_head_queue(app):  # Ide til inputliste fikk jeg av lillebror Gard Bergo Aarvik
    remove_illegal_direction(app)
    if app.direction_queue:
        app.direction = app.direction_queue.pop(0)

    return move_head(app.head_pos, app.direction, app.retningsverdi)


def remove_illegal_direction(app):
    if app.direction_queue:
        if (
            app.direction_queue[0] == app.direction
            or app.direction_queue[0] == app.motsatt_himmelretning[app.direction]
        ):
            del app.direction_queue[0]
            remove_illegal_direction(app)


def subtract_one_from_all_positives(grid):
    antall_rader = len(grid)
    for r in range(antall_rader):
        antall_kolonner = len(grid[r])
        for k in range(antall_kolonner):
            if grid[r][k] > 0:
                grid[r][k] -= 1
    return grid


def add_apple_at_random_location(grid):
    tomme_ruter = []
    antall_rader = len(grid)
    antall_kolonner = len(grid[0])
    for r in range(antall_rader):
        for k in range(antall_kolonner):
            if grid[r][k] == 0:
                tomme_ruter.append((r, k))
    pos_nytt_eple = random.choice(tomme_ruter)
    grid[pos_nytt_eple[0]][pos_nytt_eple[1]] = -1
    return grid


def is_legal_move(pos, board):
    antall_rader = len(board)
    antall_kolonner = len(board[0])
    if pos[0] < 0 or pos[1] < 0 or pos[0] >= antall_rader or pos[1] >= antall_kolonner:
        return False
    elif (
        board[pos[0]][pos[1]] > 1
    ):  # verdien skal settes til enten '1' eller '0'. Dersom verdien settes til 1 vil spillet la en slange nesten bite seg, men siden halen til slangen beveger seg like fort som hodet vil den ikke greie det. Derosm verdien settes til 0 vil slangens hode bevege seg fortere enn halen.
        return False
    else:
        return True


def redraw_all(app, canvas):
    # Visningen.
    # Denne funksjonen tegner vinduet. Funksjonen kalles hver gang
    # modellen har endret seg, eller vinduet har forandret størrelse.
    # Funksjonen kan __lese__ variabler fra app, men har ikke lov til
    # å endre på dem.
    canvas.create_rectangle(0, 0, app.width, app.height, fill="dark green")
    if app.info_mode:
        canvas.create_text(
            app.width / 2,
            10,
            text=(f"{app.head_pos=} {app.snake_size=} {app.direction=} {app.state=}"),
            font="Verdana 8",
        )
    if app.state == "startscreen":
        canvas.create_text(
            app.width / 2,
            app.width / 2,
            text=(
                f'Snake\nMode: {app.difficulty}\n\n[Start spill: trykk space]\n["E" for easy. "H" for hard]'
            ),
            font="Verdana 15",
        )
    elif app.state == "active":
        draw_board(
            canvas, 45, 65, app.width - 45, app.height - 25, app.board, app.info_mode
        )
        canvas.create_text(
            app.width / 2,
            35,
            text=(f"{app.difficulty} mode   Score: {app.score}"),
            font="Verdana 15",
        )
    elif app.state == "write_name":
        canvas.create_text(
            app.width / 2,
            app.height / 2 - 40,
            text=(f"Skriv navnet ditt\nScore: {app.score}\n\n{app.name}"),
            font="Verdana 20",
        )
    elif app.state == "scoreboard":
        canvas.create_text(
            app.width / 2, 40, text=(f"Scoreboard:\nNavn    Score"), font="Verdana 20"
        )
        for i in range(1, min(len(app.scores), 5) + 1):
            canvas.create_text(
                app.width / 2,
                50 + i * 50,
                text=(f"{app.scores[-i][1]}    {app.scores[-i][0]}"),
                font="Verdana 15",
            )

    elif app.state == "gameover":
        canvas.create_text(
            app.width / 2,
            10 + (app.height - 10) / 2,
            text=(f"Game Over\nMode: {app.difficulty}\nScore: {app.score}"),
            font="Verdana 20",
        )


if __name__ == "__main__":
    from uib_inf100_graphics.event_app import run_app

    run_app(width=500, height=400, title="Snake")
