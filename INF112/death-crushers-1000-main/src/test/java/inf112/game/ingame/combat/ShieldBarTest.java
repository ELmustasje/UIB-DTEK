package inf112.game.ingame.combat;

import inf112.gfx.DrawableType;
import inf112.utils.Vec2;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class ShieldBarTest {

    ShieldBar bar;

    @BeforeEach
    void setUp() {
        this.bar = new ShieldBar(new Vec2(0, 0), 1, 20, true);
    }

    @Test
    void testSetWidth() {
        bar.setWidth(5);
        assertEquals(50, bar.width());
    }

    @Test
    void testType() {
        assertEquals(bar.type(), DrawableType.SHIELD_BAR);
    }

    @Test
    void testCalculateWidth() {
        int shield = 10;
        int expectedWidth = Math.min(shield * 10, 200);
        ShieldBar newBar = new ShieldBar(new Vec2(0, 0), shield, 20, true);
        assertEquals(expectedWidth, newBar.width());
    }

    @Test
    void testWidth() {
        assertEquals(10, bar.width());
    }

    @Test
    void testHeight() {
        assertEquals(20, bar.height());
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
