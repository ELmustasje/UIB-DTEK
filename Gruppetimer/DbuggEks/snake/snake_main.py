def app_started(app):
    # Modellen.
    # Denne funksjonen kalles én gang ved programmets oppstart.
    # Her skal vi __opprette__ variabler i som behøves i app.
    app.direction = "east"
    app.info_mode = True


def timer_fired(app):
    # En kontroller.
    # Denne funksjonen kalles ca 10 ganger per sekund som standard.
    # Funksjonen kan __endre på__ eksisterende variabler i app.
    ...


def key_pressed(app, event):
    # En kontroller.
    # Denne funksjonen kalles hver gang brukeren trykker på tastaturet.
    # Funksjonen kan __endre på__ eksisterende variabler i app.
    match event.key:
        case "i":
            app.info_mode = not app.info_mode
        case "Up":
            app.direction = "north"
        case "Down":
            app.direction = "south"
        case "Left":
            app.direction = "west"
        case "Right":
            app.direction = "east"


def redraw_all(app, canvas):
    # Visningen.
    # Denne funksjonen tegner vinduet. Funksjonen kalles hver gang
    # modellen har endret seg, eller vinduet har forandret størrelse.
    # Funksjonen kan __lese__ variabler fra app, men har ikke lov til
    # å endre på dem.
    if app.info_mode:
        canvas.create_text(app.width / 2, 20, text=app.direction)


if __name__ == "__main__":
    from uib_inf100_graphics.event_app import run_app

    run_app(width=500, height=400, title="Snake")
