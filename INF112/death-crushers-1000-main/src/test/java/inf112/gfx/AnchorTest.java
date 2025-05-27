package inf112.gfx;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

class AnchorTest {

    @Test
    void testEnumValues() {
        Anchor[] expectedValues = { Anchor.START, Anchor.CENTER, Anchor.END };
        assertArrayEquals(expectedValues, Anchor.values());
    }

    @Test
    void testValueOf() {
        assertEquals(Anchor.START, Anchor.valueOf("START"));
        assertEquals(Anchor.CENTER, Anchor.valueOf("CENTER"));
        assertEquals(Anchor.END, Anchor.valueOf("END"));
    }

    @Test
    void testInvalidValueOfThrowsException() {
        assertThrows(IllegalArgumentException.class, () -> Anchor.valueOf("INVALID"));
    }
}
