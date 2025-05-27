package inf112.game;

import inf112.core.Controller;
import inf112.core.Core;
import inf112.core.state.StateManager;
import inf112.game.splash.SplashState;
import inf112.utils.AudioManager;

public class Game extends Core {

    StateManager gameState;
    AudioManager audio;
    Controller controller;

    public Game(int width, int height) {
        super(width, height);
    }

    @Override
    public void create() {
        super.create();
        gameState = new StateManager(new SplashState());
        audio = AudioManager.getInstance();
        controller = new Controller();
    }

    @Override
    public void render() {
        super.render();

        // Update audio event timers
        audio.update();

        // Update current game state
        gameState.update();

        // Draw all stages drawables from current state
        canvas.render(gameState.drawables());

        // Trigger callbacks for all interactable items
        controller.update(gameState.interactables());

        // End drawing and clean state before new iteration
        gameState.clear();
    }

    @Override
    public void dispose() {
        super.dispose();
    }
}
