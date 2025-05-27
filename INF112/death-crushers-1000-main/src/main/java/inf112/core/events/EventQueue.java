package inf112.core.events;

import inf112.utils.Timer;

import java.util.ArrayList;
import java.util.List;

/**
 * A queue with callbacks that run until they signal to stop or a timeout
 * occurs.
 * 
 * @see Event how to create events.
 */
public class EventQueue {

    // Encapsulate event callback and timeout
    class EventWrapper {
        float timeout;
        Event event;
        Timer timer;

        public EventWrapper(Event event, float timeout) {
            this.timeout = timeout;
            this.event = event;

            timer = new Timer();
            if (timeout > 0)
                timer.start(timeout);
        }

        public boolean run() {
            if (timeout > 0) {
                timer.update();
                return event.run() || timer.done();
            }

            return event.run();
        }
    }

    List<EventWrapper> queue;

    public EventQueue() {
        queue = new ArrayList<>();
    }

    /**
     * Add event to the end of the queue.
     * 
     * @param event Event callback
     */
    public void add(Event event) {
        queue.add(new EventWrapper(event, -1));
    }

    /**
     * Add event with timeout. The event may end early by itself, but will be
     * terminated by EventQueue after the timeout.
     * 
     * @param event   Event callback
     * @param timeout timeout in seconds
     */
    public void add(Event event, float timeout) {
        queue.add(new EventWrapper(event, timeout));
    }

    /**
     * Returns true if the internal queue is empty, signaling that all events have
     * been fired.
     * 
     * @return true if no events are left in the queue
     */
    public boolean done() {
        return queue.isEmpty();
    }

    /** Runs the current event. Does nothing if no events are enqueued. */
    public void update() {
        if (queue.size() == 0)
            return;

        EventWrapper event = queue.get(0);
        if (event.run()) {
            queue.remove(0);
        }
    }
}
