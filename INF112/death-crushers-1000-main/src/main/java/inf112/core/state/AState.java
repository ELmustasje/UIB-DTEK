package inf112.core.state;

import java.util.ArrayList;
import java.util.List;

import inf112.gfx.Container;
import inf112.gfx.Drawable;
import inf112.gfx.Interactable;

public abstract class AState implements IState {

    private final List<Drawable> drawables;
    private final List<Interactable> interactables;
    private final List<Interactable> uiComponents;

    public AState() {
        drawables = new ArrayList<>();
        interactables = new ArrayList<>();
        uiComponents = new ArrayList<>();
    }

    /**
     * Stage drawable object to be drawn by Canvas.
     * 
     * @param drawable
     */
    protected void stage(Drawable drawable) {
        if (drawable == null) {
            throw new NullPointerException("Drawable cannot be null.");
        }
        drawables.add(drawable);
    }

    /**
     * Stage interactable object to be handled by Controller.
     * 
     * @param interactable
     */
    protected void stage(Interactable interactable) {
        if (interactable == null) {
            throw new NullPointerException("Interactable cannot be null.");
        }
        drawables.add(interactable);
        interactables.add(interactable);
    }

    /**
     * Stage conatainer and its items to be handled by Controller.
     * 
     * @param container
     */
    protected void stage(Container container) {
        if (container == null) {
            throw new NullPointerException("Container cannot be null.");
        }
        interactables.addAll(container.interactables());
        drawables.addAll(container.interactables());
        drawables.addAll(container.drawables());
    }

    /**
     * Stages all drawables and interactables from child state.
     * 
     * @param state
     */
    protected void stageChild(IState state) {
        drawables.addAll(state.drawables());
        interactables.addAll(state.interactables());
    }

    /**
     * Store a UI component which persists for the lifetime of the state.
     *
     * @param interactable
     */
    protected void addComponent(Interactable interactable) {
        uiComponents.add(interactable);
    }

    @Override
    public void clear() {
        drawables.clear();
        interactables.clear();
    }

    @Override
    public List<Drawable> drawables() {
        drawables.addAll(uiComponents);
        return drawables;
    }

    @Override
    public List<Interactable> interactables() {
        interactables.addAll(uiComponents);
        return interactables;
    }
}
