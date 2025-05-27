package inf112.gfx;

import static org.junit.jupiter.api.Assertions.assertEquals;

import inf112.game.map.nodes.MapNode;
import inf112.game.map.nodes.NodeType;
import org.junit.jupiter.api.Test;
import com.badlogic.gdx.graphics.Color;
import org.mockito.Mockito;

public class PaletteTest {

    /**
     * Hjelpemetode for å sammenligne to farger.
     * Den konverterer expectedHex til en Color og sjekker hver RGBA-komponent.
     */
    private void assertColorEquals(String expectedHex, Color actual) {
        Color expected = Color.valueOf(expectedHex);
        float delta = 0.0001f;
        assertEquals(expected.r, actual.r, delta, "Rød komponent mismatch for " + expectedHex);
        assertEquals(expected.g, actual.g, delta, "Grønn komponent mismatch for " + expectedHex);
        assertEquals(expected.b, actual.b, delta, "Blå komponent mismatch for " + expectedHex);
        assertEquals(expected.a, actual.a, delta, "Alpha komponent mismatch for " + expectedHex);
    }

    @Test
    public void testPaletteColors() {
        // Kartlegger fargekonstanter til forventede hex-verdier:
        assertColorEquals("#172038", Palette.BACKGROUND);
        assertColorEquals("#ebede9", Palette.FOREGROUND);
        assertColorEquals("#c7cfcc", Palette.FOREGROUND_MUTED);
        assertColorEquals("#c7cfcc", Palette.EDGE);
        assertColorEquals("#75a743", Palette.NODE_START);
        assertColorEquals("#a53030", Palette.NODE_BATTLE);
        assertColorEquals("#73bed3", Palette.NODE_SHOP);
        assertColorEquals("#4f8fba", Palette.NODE_REST);
        assertColorEquals("#be772b", Palette.NODE_TREASURE);
        assertColorEquals("#a23e8c", Palette.NODE_ELITE);
        assertColorEquals("#202e37", Palette.NODE_BOSS);
        assertColorEquals("#819796", Palette.NODE_DEFAULT);
        assertColorEquals("#253a5e", Palette.BUTTON);
        assertColorEquals("#3c5e8b", Palette.BUTTON_HOVER);
        assertColorEquals("#ebede9", Palette.BUTTON_TEXT);
        assertColorEquals("#4f8fba", Palette.SLIDER);
        assertColorEquals("#3c5e8b", Palette.SLIDER_BUTTON);
        assertColorEquals("#75a743", Palette.CHECKBOX);
        assertColorEquals("#468232", Palette.CHECKBOX_HOVER);
        assertColorEquals("#a8b5b2", Palette.TEXTBOX_ACTIVE);
        assertColorEquals("#577277", Palette.TEXTBOX_UNACTIVE);
    }

    @Test
    void testNodeColors() {
        assertNodeColor(NodeType.START, Palette.NODE_START);
        assertNodeColor(NodeType.BATTLE, Palette.NODE_BATTLE);
        assertNodeColor(NodeType.SHOP, Palette.NODE_SHOP);
        assertNodeColor(NodeType.REST, Palette.NODE_REST);
        assertNodeColor(NodeType.TREASURE, Palette.NODE_TREASURE);
        assertNodeColor(NodeType.BOSS, Palette.NODE_BOSS);
    }

    private void assertNodeColor(NodeType type, Color expectedColor) {
        MapNode mockNode = Mockito.mock(MapNode.class);
        Mockito.when(mockNode.getNodeType()).thenReturn(type);

        assertEquals(expectedColor, Palette.getNodeColor(mockNode),
                "Feil farge returnert for nodetype: " + type);
    }
}
