package inf112.gfx;

import com.badlogic.gdx.graphics.Color;

/**
 * A Drawable object is one that has a corresponding sprite which can be drawn
 * by Canvas.
 */
public interface Drawable extends BoundingBox {

    /**
     * Get the drawable object's sprite.
     * 
     * @return sprite
     */
    default Sprite sprite() {
        return null;
    }

    /**
     * Get the drawables text if text type.
     * 
     * @return
     */
    default String text() {
        return null;
    }

    /**
     * Get the sprite scale. Sprite is centered when scaled.
     * 
     * @return
     */
    default float scale() {
        return 1f;
    }

    default Color color() {
        return Color.WHITE;
    }

    DrawableType type();
}
