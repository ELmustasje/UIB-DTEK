package no.uib.inf101.sample.sprites;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class SpriteTest {

    @Test
    void sanityTest(){
        ISprite sprite = new Sprite(null);
        assertEquals(400,sprite.getHeight());
        assertEquals(150,sprite.getWidth());
        assertEquals(0,sprite.getStrength());
        assertFalse(sprite.isMirrored());
    }

    @Test
    void testSetValues(){
        ISprite sprite = new Sprite(null);
        sprite.setValues(1,1,1,1);
        assertEquals(1,sprite.getX());
        assertEquals(1,sprite.getY());
        assertEquals(1,sprite.getWidth());
        assertEquals(1,sprite.getHeight());
    }

    @Test
    void testUpdateStrength(){
        ISprite sprite = new Sprite(null);
        assertEquals(0,sprite.getStrength());
        sprite.updateStrength(1);
        assertEquals(1,sprite.getStrength());
        sprite.updateStrength(999);
        assertEquals(10,sprite.getStrength());
    }

    @Test
    void testGetValues(){
        ISprite sprite = new Sprite(null);
        sprite.setValues(1,2,3,4);
        assertEquals(1,sprite.getX());
        assertEquals(2,sprite.getY());
        assertEquals(3,sprite.getWidth());
        assertEquals(4,sprite.getHeight());
    }

    @Test
    void testMirrorTurnAndIsMirrored(){
        ISprite sprite = new Sprite(null);
        sprite.setDefaultValues();
        assertFalse(sprite.isMirrored());
        sprite.mirrorTurn(true);
        assertTrue(sprite.isMirrored());
    }
}
