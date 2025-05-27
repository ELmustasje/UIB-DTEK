package inf112.core.state;

import java.util.List;

import inf112.gfx.Drawable;
import inf112.gfx.Interactable;
import inf112.utils.Timer;

/**
 * The StateManager keeps track of one current running state and calls init and
 * clean before/after switching to another state.
 * NOTE: constructor calls init() for initial state!
 */
public class StateManager implements IState {

    IState state;
    Timer timer;
    IState nextState;

    public StateManager(IState initialState) {
        timer = new Timer();
        state = initialState;

        // Last
        state.init(this);
    }

    @Override
    public void init(StateManager manager) {

    }

    /**
     * Immediately switches to newState.
     * 
     * @param newState state to switch to
     */
    public void switchTo(IState newState) {
        // Reset timer here to avoid cases where state switch happens before a timeout.
        timer.reset();

        state.clean();
        state = newState;
        newState.init(this);
    }

    /**
     * Switches to another state after a set timeout. Can safely be called multiple
     * times, the timer only activates once.
     * 
     * @param newState state to switch to
     * @param seconds  number of elapsed seconds before switch
     */
    public void awaitSwitch(IState newState, float seconds) {
        if (!timer.running()) {
            timer.start(seconds);
            nextState = newState;
        }
    }

    /**
     * Updates current state. Should be called each frame.
     * 
     * @param canvas Core canvas to draw to
     */
    @Override
    public void update() {
        state.update();

        if (timer.running()) {
            timer.update();
            if (timer.done()) {
                switchTo(nextState);
            }
        }
    }

    /** Cleans up current state. Call at end of managers lifetime. */
    @Override
    public void clean() {
        state.clean();
    }

    @Override
    public List<Drawable> drawables() {
        return state.drawables();
    }

    @Override
    public List<Interactable> interactables() {
        return state.interactables();
    }

    @Override
    public void clear() {
        state.clear();
    }
}
