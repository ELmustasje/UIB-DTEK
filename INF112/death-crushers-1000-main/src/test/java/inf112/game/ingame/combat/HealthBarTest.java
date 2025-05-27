
package inf112.game.ingame.combat;

import inf112.gfx.DrawableType;
import inf112.utils.Vec2;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class HealthBarTest {

    HealthBar bar;

    @BeforeEach
    void setUp() {
        this.bar = new HealthBar(new Vec2(0, 0), 1, 20, true);
    }

    @Test
    void testSetWidth() {
        bar.setWidth(5);
        assertEquals(5, bar.width());
    }

    @Test
    void testType() {
        assertEquals(bar.type(), DrawableType.HEALTH_BAR);
    }

    @Test
    void testProgress() {
        bar.setProgress(5, 10);
        assertEquals(0.5f, bar.getProgress());
    }

    @Test
    void testWidth() {
        assertEquals(320, bar.width());
    }

    @Test
    void testHeight() {
        assertEquals(1, bar.height());
    }

    @Test
    void testPos() {
        Vec2 expectedPos = new Vec2(0, 0);
        assertEquals(expectedPos, bar.pos());
    }

    @Test
    void testDrawables() {
        assertEquals(1, bar.drawables().size());
    }

    @Test
    void testInteractables() {
        assertEquals(0, bar.interactables().size());
    }
}
