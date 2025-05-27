package inf112.core.events;

/**
 * An Event, together with {@link EventQueue}, enables the user to easily add
 * multiple callbacks to a list that will run in order, and with a set time
 * interval if specified.
 * 
 * <pre>{@code
 * 
 * // Example: moving a circle until it reaches a target position
 * queue.add(() -> {
 *     moveCircleToTarget();
 *     return circleIsAtTarget();
 * });
 * 
 * }</pre>
 */
public interface Event {

    /**
     * Runs every frame the event is active. Should return false once the event is
     * done. Blocks next events in queue while running.
     * 
     * @return true if done
     */
    boolean run();
}
