package inf112.gfx;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class CursorTypeTest {

    @Test
    public void testCursorTypeValues() {
        // Sjekk at enum-verdiene eksisterer
        assertEquals(2, CursorType.values().length);
        assertEquals(CursorType.NORMAL, CursorType.valueOf("NORMAL"));
        assertEquals(CursorType.POINTER, CursorType.valueOf("POINTER"));
    }

    @Test
    public void testEnumToString() {
        assertEquals("NORMAL", CursorType.NORMAL.toString());
        assertEquals("POINTER", CursorType.POINTER.toString());
    }
}
