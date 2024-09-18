package no.uib.inf101.sample.equipment.flappyBird;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

public class FlappyBirdTest {

    @Test
    void testGetScore(){
        FlappyBird game = new FlappyBird(null);
        assertEquals(0,game.getScore());
    }

    @Test
    void testIsRunning(){
        FlappyBird game = new FlappyBird(null);
        assertFalse(game.isRunning());
        game.run();
        assertTrue(game.isRunning());
    }

    @Test
    void testGetSpeed(){
        FlappyBird game = new FlappyBird(null);
        assertEquals(2,game.getSpeed());
    }

    @Test
    void testGetGameHeigth(){
        FlappyBird game = new FlappyBird(null);
        assertEquals(214,game.getGameHeigth());
    }

    @Test
    void testGetGameY(){
        FlappyBird game = new FlappyBird(null);
        assertEquals(151,game.getGameY());
    }

    @Test
    void testReset(){
        FlappyBird game = new FlappyBird(null);
        assertFalse(game.isRunning());
        game.run();
        assertTrue(game.isRunning());
        game.reset();
        assertFalse(game.isRunning());
    }

    @Test
    void testRun(){
        FlappyBird game = new FlappyBird(null);
        assertFalse(game.isRunning());
        game.run();
        assertTrue(game.isRunning());
    }

    @Test
    void testDone(){
        FlappyBird game = new FlappyBird(null);
        assertFalse(game.done());
    }

}
