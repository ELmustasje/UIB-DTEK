package inf112.core;

import java.util.List;

import com.badlogic.gdx.Input;

import inf112.gfx.Canvas;
import inf112.gfx.Interactable;
import inf112.utils.Keyboard;
import inf112.utils.Mouse;

public class Controller {

    public Controller() {
    }

    /**
     * Update controller, calling all interactable hover and click callbacks.
     * 
     * @param interactables
     */
    public void update(List<Interactable> interactables) {
        for (Interactable i : interactables) {
            boolean hovered = Mouse.over(i);
            boolean clicked = hovered && Mouse.clicked();
            i.click(clicked);
            i.hover(hovered);
        }
    }

    /**
     * 
     */
    public static void update(float deltaTime, float minX, float maxX) {
        float scrollDirection = 0f;
        if (Keyboard.down(Input.Keys.LEFT) || Keyboard.down(Input.Keys.A)) {
            scrollDirection -= 1f;
        }
        if (Keyboard.down(Input.Keys.RIGHT) || Keyboard.down(Input.Keys.D)) {
            scrollDirection += 1f;
        }

        if (scrollDirection != 0f) {
            Canvas.getInstance().camController().update(deltaTime, scrollDirection, minX, maxX);
        }
    }
}
