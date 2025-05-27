package inf112.core.events;

import java.util.HashMap;
import java.util.Map;

/**
 * Once is a simple hasmap wrapper that makes it easier to do things only once
 * inside a loop.
 */
public class Once {

    Map<String, Boolean> map;

    public Once() {
        map = new HashMap<>();
    }

    /**
     * Returns true if the tag has never been used before (or after reset).
     * 
     * @param tag tag to identify once
     * @return true if only called once
     */
    public boolean once(String tag) {
        boolean prev = map.getOrDefault(tag, false);
        map.put(tag, true);
        return prev;
    }

    /**
     * Reset the internal map. All once calls will returns true again.
     */
    public void reset() {
        map.clear();
    }
}
