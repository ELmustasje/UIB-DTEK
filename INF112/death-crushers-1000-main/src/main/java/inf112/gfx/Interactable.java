package inf112.gfx;

public interface Interactable extends Drawable {

    /**
     * Called every frame. Gives the current hover state for the Interactable.
     * Should return true if the mouse cursor sprite should be shown Should return
     * true if the mouse pointer (hand) sprite should be shown.
     * 
     * @param isHovered
     * @return true if switch to pointer
     */
    boolean hover(boolean isHovered);

    void click(boolean isClicked);

}
