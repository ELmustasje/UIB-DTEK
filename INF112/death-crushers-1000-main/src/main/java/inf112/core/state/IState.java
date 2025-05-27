package inf112.core.state;

import inf112.gfx.Container;

/**
 * A state is a class with a self contained environment that handles logic for
 * one specific game state. The state can use the given manager to switch to
 * another state, effectively ending its lifetime. The StateManager handles init
 * and clean, therefore no methods of the state class should be called
 * directly.
 */
public interface IState extends Container {

    /**
     * Runs once at beginning of state switch.
     * 
     * @param manager The parent StateManager object. Use this to switch to other
     *                states internally.
     */
    void init(StateManager manager);

    /**
     * Runs every frame while state is active.
     * 
     * @param canvas core canvas to draw to
     */
    void update();

    /** Runs once before switching to another state. */
    void clean();

    /**
     * Clear internal lists for render components.
     */
    void clear();
}
