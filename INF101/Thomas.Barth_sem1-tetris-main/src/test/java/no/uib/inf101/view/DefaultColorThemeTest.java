package no.uib.inf101.view;

import no.uib.inf101.tetris.view.ColorTheme;
import no.uib.inf101.tetris.view.DefaultColorTheme;
import org.junit.Test;

import java.awt.*;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class DefaultColorThemeTest {
    @Test
    public void sanityDefaultColorThemeTest() {
        ColorTheme colors = new DefaultColorTheme();
        assertEquals(null, colors.getBackgroundColor());
        assertEquals(new Color(0, 0, 0, 0), colors.getFrameColor());
        assertEquals(Color.BLACK, colors.getCellColor('-'));
        assertEquals(Color.RED, colors.getCellColor('J'));
        assertThrows(IllegalArgumentException.class, () -> colors.getCellColor('\n'));
    }
}
