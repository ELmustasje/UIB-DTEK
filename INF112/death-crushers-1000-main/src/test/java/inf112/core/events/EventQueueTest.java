package inf112.core.events;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Graphics;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class EventQueueTest {

    static int counter;

    @BeforeEach
    void resetCounter() {
        counter = 0;
    }

    @BeforeEach
    void setupGdx() {
        Gdx.graphics = mock(Graphics.class);
        when(Gdx.graphics.getDeltaTime()).thenReturn(0.016f); // Simulating 16ms/frame (60 FPS)
    }

    @Test
    void testSingleEventExecutesOnce() {
        EventQueue queue = new EventQueue();
        Event event = () -> {
            counter++;
            return true;
        };

        queue.add(event);
        queue.update();

        assertEquals(1, counter, "Event should execute exactly once.");
        assertTrue(queue.done(), "Queue should be empty after event execution.");
    }

    @Test
    void testMultipleEventsExecuteInOrder() {
        EventQueue queue = new EventQueue();
        final int count = 5;

        for (int i = 0; i < count; i++) {
            queue.add(() -> {
                counter++;
                return true;
            });
        }

        while (!queue.done()) {
            queue.update();
        }

        assertEquals(count, counter, "All events should have executed in order.");
        assertTrue(queue.done(), "Queue should be empty after all events execute.");
    }

    @Test
    void testEventQueueWithTimeout() {
        EventQueue queue = new EventQueue();
        Event event = mock(Event.class);
        when(event.run()).thenReturn(false); // Event never completes on its own

        queue.add(event, 1); // Add event with a 1-second timeout

        for (int i = 0; i < 100; i++) {
            queue.update();
            if (queue.done())
                break;
        }

        assertTrue(queue.done(), "Queue should be empty after timeout.");
        verify(event, atLeastOnce()).run(); // Ensure event was attempted
    }

    @Test
    void testEventCompletesBeforeTimeout() {
        EventQueue queue = new EventQueue();
        Event event = mock(Event.class);
        when(event.run()).thenReturn(true); // Event completes immediately

        queue.add(event, 5); // Timeout is long, but event finishes early
        queue.update();

        assertTrue(queue.done(), "Queue should be empty after event completes.");
        verify(event, times(1)).run(); // Ensure event ran exactly once
    }

    @Test
    void testEmptyQueueDoesNotCrash() {
        EventQueue queue = new EventQueue();

        assertTrue(queue.done(), "Queue should start empty.");
        queue.update(); // Should do nothing
        assertTrue(queue.done(), "Queue should remain empty.");
    }

    @Test
    void testLongRunningEventWithTimeout() {
        EventQueue queue = new EventQueue();
        Event event = mock(Event.class);
        when(event.run()).thenReturn(false); // Event never completes on its own

        queue.add(event, 2); // 2-second timeout

        for (int i = 0; i < 200; i++) { // Simulating 200 frames (~3.2 sec)
            queue.update();
            if (queue.done())
                break;
        }

        assertTrue(queue.done(), "Queue should be empty after timeout.");
        verify(event, atLeastOnce()).run();
    }

    @Test
    void testMultipleEventsWithTimeouts() {
        EventQueue queue = new EventQueue();
        Event event1 = mock(Event.class);
        Event event2 = mock(Event.class);
        when(event1.run()).thenReturn(false);
        when(event2.run()).thenReturn(false);

        queue.add(event1, 1); // 1 sec timeout
        queue.add(event2, 2); // 2 sec timeout

        for (int i = 0; i < 300; i++) { // Simulating 300 frames (~4.8 sec)
            queue.update();
            if (queue.done())
                break;
        }

        assertTrue(queue.done(), "Queue should be empty after both timeouts.");
        verify(event1, atLeastOnce()).run();
        verify(event2, atLeastOnce()).run();
    }

    @Test
    void testEventsWithMixedCompletionBehavior() {
        EventQueue queue = new EventQueue();

        Event quickEvent = mock(Event.class);
        when(quickEvent.run()).thenReturn(true); // Completes immediately

        Event longEvent = mock(Event.class);
        when(longEvent.run()).thenReturn(false); // Never completes on its own

        queue.add(quickEvent);
        queue.add(longEvent, 2); // 2-second timeout

        for (int i = 0; i < 300; i++) { // Simulating 300 frames (~4.8 sec)
            queue.update();
            if (queue.done())
                break;
        }

        assertTrue(queue.done(), "Queue should be empty after processing both events.");
        verify(quickEvent, times(1)).run(); // Quick event runs once
        verify(longEvent, atLeastOnce()).run(); // Long event should be attempted multiple times
    }

    @Test
    void testQueueProcessesEventsSequentially() {
        EventQueue queue = new EventQueue();
        Event event1 = mock(Event.class);
        Event event2 = mock(Event.class);

        when(event1.run()).thenReturn(false, false, true); // Completes on third call
        when(event2.run()).thenReturn(true); // Completes immediately

        queue.add(event1);
        queue.add(event2);

        queue.update();
        assertFalse(queue.done(), "Queue should still have event2 pending.");

        queue.update();
        queue.update();
        queue.update();
        System.out.println(queue.queue);
        assertTrue(queue.done(), "Queue should be empty after all events execute.");

        verify(event1, times(3)).run();
        verify(event2, times(1)).run();
    }

    @Test
    void testRapidUpdateCalls() {
        EventQueue queue = new EventQueue();
        Event event = mock(Event.class);
        when(event.run()).thenReturn(true);

        queue.add(event);

        for (int i = 0; i < 50; i++) { // Rapid updates in succession
            queue.update();
        }

        assertTrue(queue.done(), "Queue should be empty even with rapid updates.");
        verify(event, times(1)).run();
    }
}
