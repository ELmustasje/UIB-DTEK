package inf112.utils;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

public class TimerTest {
    private Timer timer;

    @BeforeEach
    void setUp() {
        timer = new Timer();
    }

    @Test
    void testTimerStartsCorrectly() {
        timer.start(5.0f);
        assertTrue(timer.running(), "Timer should be running after start");
        timer.stop();
        assertFalse(timer.running(), "Timer should be stopped after stop");
        timer.start(0);
        assertTrue(timer.running(), "Timer should be running after start");
    }

    @Test
    void testTimerOn0() {
        timer.start(0.0f);
        assertTrue(timer.running(), "Timer should be running after start");
        timer.stop();
        assertFalse(timer.running(), "Timer should be stopped after stop");
        timer.start(5.0f);
        assertTrue(timer.running(), "Timer should be running after start");
    }

    @Test
    void testTimerRestart() {
        timer.start(5.0f);
        assertTrue(timer.running(), "Timer should be running after start");
        timer.stop();
        assertFalse(timer.running(), "Timer should be stopped after stop");
        timer.reset();
        assertFalse(timer.running(), "Timer should be stopped after reset");
    }

    @Test
    void testTimerDoesNotRestartIfAlreadyRunning() {
        timer.start(5.0f);
        timer.start(10.0f); // Should not restart
        assertTrue(timer.running(), "Timer should be running after start");
        assertEquals(timer.getTimeRemaining(), 5);
        timer.stop();
        timer.start(10);
        assertEquals(timer.getTimeRemaining(), 10);

    }

    @Test
    void testTimerStopsCorrectly() {
        timer.start(5.0f);
        timer.stop();
        assertFalse(timer.running(), "Timer should stop when stop() is called");
        timer.start(10);
        assertEquals(10, timer.getTimeRemaining());
        assertTrue(timer.running(), "Timer should be running after stop");
    }

    @Test
    void testTimerDone() {
        timer.start(0.0f);
        timer.stop();
        assertTrue(timer.done(), "Timer should be done after stop");
    }

    @Test
    void testTimerResetsCorrectly() {
        timer.start(5.0f);
        timer.reset();
        assertFalse(timer.running(), "Timer should not be running after reset");
    }
}
