package inf112.gfx;

import inf112.utils.Vec2;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

public class DrawableTest {

    // En testklasse som implementerer Drawable
    private static class TestDrawable implements Drawable {
        private final DrawableType type;

        public TestDrawable(DrawableType type) {
            this.type = type;
        }

        @Override
        public DrawableType type() {
            return type;
        }

        @Override
        public float width() {
            return 0;
        }

        @Override
        public float height() {
            return 0;
        }

        @Override
        public Vec2 pos() {
            return null;
        }
    }

    @Test
    public void testDefaultMethods() {
        Drawable drawable = new TestDrawable(DrawableType.SPRITE);

        // Sjekk at default-metodene returnerer forventede verdier
        assertNull(drawable.sprite());
        assertNull(drawable.text());
        assertEquals(1, drawable.scale());
    }

    @Test
    public void testTypeMethod() {
        Drawable drawable = new TestDrawable(DrawableType.TEXT);

        // Sjekk at type() returnerer riktig type
        assertEquals(DrawableType.TEXT, drawable.type());
    }
}
