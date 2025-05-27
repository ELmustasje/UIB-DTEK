package inf112.core.events;

import java.util.ArrayList;

/**
 * A WaitGroup is a pool of events. All events are run at the same time, and
 * are removed when done. WaitGroup is done when all events have finished.
 */
public class WaitGroup {

    ArrayList<Event> group;
    EventQueue queue;

    public WaitGroup() {
        group = new ArrayList<>();
        queue = new EventQueue();
    }

    /**
     * Add event to wait group.
     * 
     * @param event
     */
    public void add(Event event) {
        queue.add(() -> {
            group.add(event);
            return true;
        });
    }

    /**
     * Add event to group after a given delay. Delay is seconds after function call.
     * 
     * @param event added after delay
     * @param delay delay before adding in seconds
     */
    public void addWithDelay(Event event, float delay) {
        queue.add(() -> false, delay);
        add(event);
    }

    /** Run all events and update internal queue. */
    public void update() {
        queue.update();
        group.removeIf(Event::run);
    }

    /**
     * Checks to see if events have finished running.
     * 
     * @return true when all events have completed
     */
    public boolean done() {
        return group.isEmpty() && queue.done();
    }
}
