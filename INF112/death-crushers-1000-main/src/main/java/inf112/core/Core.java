package inf112.core;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Graphics.DisplayMode;
import com.badlogic.gdx.Input;
import inf112.gfx.Canvas;

public class Core implements ICore {

    protected int width;
    protected int height;
    protected Canvas canvas;

    boolean fullscreen;

    public Core(int width, int height) {
        if (width <= 0 || height <= 0)
            throw new IllegalArgumentException("Width and height must be positive integers.");

        this.width = width;
        this.height = height;
        fullscreen = false;
    }

    @Override
    public void create() {
        canvas = new Canvas(width, height);
    }

    @Override
    public void dispose() {
        canvas.dispose();
    }

    @Override
    public void resize(int screenWidth, int screenHeight) {
        canvas.resize(screenWidth, screenHeight);
    }

    @Override
    public void render() {
        // Toggle fullscreen mode with 'F' (debug)
        if (Gdx.input.isKeyJustPressed(Input.Keys.F)) {
            if (fullscreen) {
                Gdx.graphics.setWindowedMode(Config.SCREEN_WIDTH, Config.SCREEN_HEIGHT);
            } else {
                DisplayMode displayMode = Gdx.graphics.getDisplayMode();
                Gdx.graphics.setFullscreenMode(displayMode);
            }

            fullscreen = !fullscreen;
        }
    }

    @Override
    public void pause() {
    }

    @Override
    public void resume() {
    }
}
