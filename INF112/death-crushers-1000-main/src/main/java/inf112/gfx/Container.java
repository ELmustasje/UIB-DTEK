package inf112.gfx;

import java.util.List;

public interface Container {

    /**
     * Get drawable components in this container.
     * 
     * @return
     */
    List<Drawable> drawables();

    /**
     * Get interactable components in this container.
     * 
     * @return
     */
    List<Interactable> interactables();

}
