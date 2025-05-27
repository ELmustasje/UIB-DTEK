package inf112.gfx.ui;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

import inf112.gfx.DrawableType;
import inf112.utils.Vec2;

public class ProgressBarTest {

    ProgressBar bar = new DummyProgressBar();

    @Test
    void testInitialValues() {
        assertNotNull(bar);
        assertEquals(10, bar.pos().x);
        assertEquals(100, bar.pos().y);
        assertEquals(100, bar.width());
        assertEquals(20, bar.height());
        assertEquals(1, bar.drawables().size(), "DummyProgressBar should have one drawable");
        assertEquals(0, bar.interactables().size(), "DummyProgressBar should not have interactables");
        assertEquals(DrawableType.SPRITE, bar.type(), "DummyProgressBar should be of type SPRITE");
    }

    @Test
    void testSetWidth() {
        bar.setWidth(200);
        assertEquals(200, bar.width());
    }

    @Test
    void testLabel() {
        assertEquals(bar.label, null, "DummyProgressBar should not have a label");
    }

    private class DummyProgressBar extends ProgressBar {
        public DummyProgressBar() {
            super(new Vec2(10, 100), 100, 20, "Progress:", true);
        }

        @Override
        public DrawableType type() {
            return DrawableType.SPRITE;
        }
    }
}
