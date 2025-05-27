package inf112.core;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class ConfigTest {

    @Test
    void testConfigValues() {
        // Sjekk at de numeriske verdiene stemmer
        assertEquals(60, Config.FPS);
        assertFalse(Config.DEBUG); // Standardverdi skal være false
        assertEquals(7, Config.MAP_COLUMNS);
        assertEquals(960, Config.SCREEN_WIDTH); // 1920 / 2
        assertEquals(540, Config.SCREEN_HEIGHT); // 1080 / 2
        assertEquals(1920, Config.VIEW_WIDTH);
        assertEquals(1080, Config.VIEW_HEIGHT);
        assertEquals(256, Config.CARD_W);
        assertEquals(416, Config.CARD_H);
        assertEquals(3, Config.STARTING_MANA);
        assertEquals(2, Config.MULTIPLE_ENEMY_MIN);
        assertEquals(4, Config.MULTIPLE_ENEMY_MAX);

        // Sjekk at string-konstanter er korrekte
        assertEquals("Death Crushers", Config.WINDOW_TITLE);
        assertEquals("Death Crushers 1000", Config.GROUP_NAME);
        assertEquals("Warden of Høytek", Config.GAME_NAME);
        assertEquals("libGDX", Config.LIBRARY);
    }

}
