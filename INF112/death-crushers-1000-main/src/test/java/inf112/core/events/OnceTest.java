package inf112.core.events;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

/**
 * Unit tests for the {@link Once} class.
 */
public class OnceTest {

    /**
     * Tests that calling {@code once()} with a new tag returns false.
     * <p>
     * This confirms that the tag is considered "unused" on its first invocation.
     */
    @Test
    public void testOnceReturnsFalseForNewTag() {
        Once once = new Once();
        assertFalse(once.once("testTag"),
                "Once should return false on the first call for a new tag.");
    }

    /**
     * Tests that calling {@code once()} twice with the same tag returns false on
     * the first call and true on subsequent calls.
     */
    @Test
    public void testOnceReturnsTrueOnSubsequentCalls() {
        Once once = new Once();

        // First call: tag has not been used and should return false.
        assertFalse(once.once("testTag"), "First call should return false.");

        // Next calls: tag has been marked as used and should return true.
        assertTrue(once.once("testTag"), "Second call should return true.");
        assertTrue(once.once("testTag"), "Third call should also return true.");
    }

    /**
     * Tests that different tags are tracked independently.
     * <p>
     * Each new tag should return false on its first call.
     */
    @Test
    public void testIndependentTags() {
        Once once = new Once();
        assertFalse(once.once("tag1"), "First call for tag1 should return false.");
        assertFalse(once.once("tag2"), "First call for tag2 should return false.");

        // Subsequent calls for each tag should return true.
        assertTrue(once.once("tag1"), "Second call for tag1 should return true.");
        assertTrue(once.once("tag2"), "Second call for tag2 should return true.");
    }

    /**
     * Tests that calling {@code reset()} clears the internal state,
     * so that the same tag behaves as if it was never used.
     */
    @Test
    public void testReset() {
        Once once = new Once();

        assertFalse(once.once("tag"), "First call should return false.");
        assertTrue(once.once("tag"), "Second call should return true.");

        once.reset();

        assertFalse(once.once("tag"), "After reset, first call should return false.");
    }
}
