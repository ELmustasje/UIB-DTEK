package inf112.utils;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Input;
import com.badlogic.gdx.math.Rectangle;
import com.badlogic.gdx.math.Vector3;

import inf112.gfx.BoundingBox;
import inf112.gfx.Canvas;

public class Mouse {

    /**
     * If the mouse button is down.
     * 
     * @return true if the mouse button is held down
     */
    public static boolean down() {
        return Gdx.input.isButtonPressed(Input.Buttons.LEFT);
    }

    /**
     * If the mouse button is clicked. Only true one frame.
     * 
     * @return true if the mouse button was just clicked
     */
    public static boolean clicked() {
        return Gdx.input.isButtonJustPressed(Input.Buttons.LEFT);
    }

    /**
     * Get mouse position in <b>world space</b>.
     * 
     * @return pos in world space
     */
    public static Vec2 pos() {

		Vector3 pos = new Vector3(Gdx.input.getX(), Gdx.input.getY(), 0);

		Canvas.getInstance().getViewport().unproject(pos);

        // Vec2 v = new Vec2(Gdx.input.getX() / scaleX, Gdx.input.getY() / scaleY);
        return new Vec2(pos.x, pos.y);
    }

    /**
     * Get mouse position in <b>screen space</b>.
     * 
     * @return pos in screen space
     */
    public static Vec2 realPos() {
        return new Vec2(Gdx.input.getX(), Gdx.input.getY());
    }

    /**
     * Checks to see if the mouse cursor is inside the bounding box.
     * 
     * @param box box to check
     * @return true if mouse is inside
     */
    public static boolean over(BoundingBox box) {
        Vec2 pos = pos();
        Rectangle b = new Rectangle(box.pos().x, box.pos().y, box.width(), box.height());
        float x = pos.x;
        float y = pos.y;
        return !(x < b.x || x > b.x + b.width || y < b.y || y > b.y + b.height);
    }
}
