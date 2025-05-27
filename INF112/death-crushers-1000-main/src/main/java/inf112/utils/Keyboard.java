package inf112.utils;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Input;
import com.badlogic.gdx.InputAdapter;

public class Keyboard {
    private static String lastTypedChar = "";
    private static boolean isReleased = true;

    static {
        Gdx.input.setInputProcessor(new InputAdapter() {
            @Override
            public boolean keyTyped(char character) {
                if (character == '\b') {
                    lastTypedChar = "BACKSPACE";
                } else {
                    lastTypedChar = String.valueOf(character);
                }

                return true;
            }
        });
    }

    /**
     * Checks if a key is currently held down.
     *
     * @param key Keycode from {@link Input.Keys}
     * @return true if the key is held down
     */
    public static boolean down(int key) {
        return Gdx.input.isKeyPressed(key);
    }

    /**
     * Checks if a key was just pressed (only true for one frame).
     *
     * @param key Keycode from {@link Input.Keys}
     * @return true if the key was just pressed
     */
    public static boolean pressed(int key) {
        return Gdx.input.isKeyJustPressed(key);
    }

    /**
     * Checks if any key is currently held down.
     *
     * @return true if any key is held down
     */
    public static boolean anyKeyDown() {
        return Gdx.input.isKeyPressed(Input.Keys.ANY_KEY);
    }

    /**
     * Checks if any key was just pressed (only true for one frame).
     *
     * @return true if any key was just pressed
     */
    public static boolean anyKeyPressed() {
        return Gdx.input.isKeyJustPressed(Input.Keys.ANY_KEY);
    }

    /**
     * Gets the last typed character.
     *
     * @return the last character typed as a String
     */
    public static String input() {
        if (isReleased) {
            isReleased = false;
            return lastTypedChar;
        } else {
            if (Gdx.input.isKeyJustPressed(Input.Keys.ANY_KEY)) {
                isReleased = true;
            }
        }
        return null;
    }
}
