package inf112.core.events;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Graphics;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

/**
 * Unit tests for the {@link WaitGroup} class.
 */
public class WaitGroupTest {

    /**
     * Sets up a mocked Gdx.graphics.
     * This is needed because Timer.update() (used in EventQueue.update())
     * calls Gdx.graphics.getDeltaTime(), and without a valid graphics context,
     * a NullPointerException is thrown.
     */
    @BeforeEach
    public void setupGdxGraphics() {
        Graphics mockGraphics = mock(Graphics.class);
        when(mockGraphics.getDeltaTime()).thenReturn(0.016f); // simulate ~60fps
        Gdx.graphics = mockGraphics;
    }

    /**
     * Tests that the WaitGroup constructor initializes an empty group and queue.
     */
    @Test
    public void testInitialization() {
        WaitGroup wg = new WaitGroup();
        assertEquals(0, wg.group.size(), "WaitGroup should have an empty event group.");
        assertEquals(0, wg.queue.queue.size(), "WaitGroup should have an empty event queue.");
    }

    /**
     * Tests that the add() method enqueues events.
     */
    @Test
    public void testAdd() {
        WaitGroup wg = new WaitGroup();
        Event event = () -> true;
        wg.add(event);
        assertEquals(1, wg.queue.queue.size(), "WaitGroup queue should have one event after first add.");
        wg.add(event);
        assertEquals(2, wg.queue.queue.size(), "WaitGroup queue should have two events after second add.");
    }

    /**
     * Tests that addWithDelay() schedules a delayed event and adds the given event
     * to the group.
     * <p>
     * After calling addWithDelay() and one update(), the group should contain the
     * immediate event,
     * but WaitGroup.done() remains false because the delayed event is still
     * pending.
     */
    @Test
    public void testAddWithDelay() {
        WaitGroup wg = new WaitGroup();
        Event event = () -> true;

        wg.addWithDelay(event, 0.1f); // 0.1 seconds delay
        wg.update();

        assertFalse(wg.done(), "WaitGroup should not be done if the delayed event is still pending.");

        // Simulate the passage of time by repeatedly calling update().
        int maxIterations = 100;
        int iterations = 0;
        while (!wg.done() && iterations < maxIterations) {
            wg.update();
            iterations++;
        }

        assertTrue(wg.done(), "WaitGroup should eventually be done after the delayed event times out.");
    }

    /**
     * Tests that update() processes the queued event and moves it into the group.
     */
    @Test
    public void testUpdate() {
        WaitGroup wg = new WaitGroup();
        Event event = mock(Event.class);
        wg.add(event);
        assertEquals(0, wg.group.size(), "WaitGroup group should have zero events before updating.");
        wg.update();
        assertEquals(1, wg.group.size(), "WaitGroup group should have one event after updating.");
    }

    /**
     * Tests that a new WaitGroup with no events is done.
     */
    @Test
    public void testDoneWithoutEvents() {
        WaitGroup wg = new WaitGroup();
        assertTrue(wg.done(), "A new WaitGroup with no events should be done.");
    }

    /**
     * Tests that an event that never completes prevents the WaitGroup from being
     * done.
     */
    @Test
    public void testNonCompletingEvent() {
        WaitGroup wg = new WaitGroup();
        Event nonCompletingEvent = () -> false; // always returns false
        wg.add(nonCompletingEvent);
        wg.update();
        assertFalse(wg.done(), "WaitGroup should not be done if an event never completes.");
    }

    /**
     * Tests that an event requiring multiple update calls eventually completes.
     * <p>
     * This event returns true only after being run twice.
     */
    @Test
    public void testEventCompletesAfterMultipleUpdates() {
        WaitGroup wg = new WaitGroup();

        // Create an event that only completes after two runs.
        class MultiRunEvent implements Event {
            private int runs = 0;

            @Override
            public boolean run() {
                runs++;
                return runs >= 2;
            }
        }
        Event multiRunEvent = new MultiRunEvent();
        wg.add(multiRunEvent);

        wg.update();
        assertFalse(wg.done(), "WaitGroup should not be done after one update for a multi-run event.");

        wg.update();
        assertTrue(wg.done(), "WaitGroup should be done after the multi-run event completes.");
    }
}
