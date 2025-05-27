package inf112.core;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Graphics;
import com.badlogic.gdx.Graphics.DisplayMode;
import com.badlogic.gdx.Input;
import inf112.gfx.Canvas;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.*;

/**
 * Test class for the Core class.
 */
public class CoreTest {
    private static final int WIDTH = 1024;
    private static final int HEIGHT = 768;

    /**
     * Tests that a Core instance is initialized with the correct dimensions and
     * default values.
     */
    @Test
    public void testCoreInitialization() {
        Core core = new Core(WIDTH, HEIGHT);
        assertEquals(WIDTH, core.width, "Width should be set correctly");
        assertEquals(HEIGHT, core.height, "Height should be set correctly");
        assertFalse(core.fullscreen, "Fullscreen should be false by default");
        assertNull(core.canvas, "Canvas should be null before creation");
    }

    /**
     * Tests that creating a Core instance with invalid dimensions throws an
     * exception.
     */
    @Test
    public void testCoreInvalidDimensions() {
        assertThrows(IllegalArgumentException.class, () -> new Core(-WIDTH, -HEIGHT));
        assertThrows(IllegalArgumentException.class, () -> new Core(-WIDTH, HEIGHT));
        assertThrows(IllegalArgumentException.class, () -> new Core(WIDTH, -HEIGHT));
        assertThrows(IllegalArgumentException.class, () -> new Core(WIDTH, 0));
        assertThrows(IllegalArgumentException.class, () -> new Core(0, HEIGHT));
        assertThrows(IllegalArgumentException.class, () -> new Core(0, 0));
    }

    /**
     * Tests that calling dispose() on Core properly disposes the canvas.
     * <p>
     * Verifies that each call to dispose() calls the canvas's dispose() method,
     * and that no extra interactions occur.
     */
    @Test
    public void testDispose() {
        Core core = new Core(WIDTH, HEIGHT);
        core.canvas = mock(Canvas.class); // Inject a mocked canvas

        // Dispose multiple times and verify cumulative calls.
        core.dispose();
        verify(core.canvas, times(1)).dispose();

        core.dispose();
        verify(core.canvas, times(2)).dispose();

        core.dispose();
        verify(core.canvas, times(3)).dispose();

        verifyNoMoreInteractions(core.canvas);

        // If canvas is null, dispose() should throw a NullPointerException.
        core.canvas = null;
        assertThrows(NullPointerException.class, core::dispose);
    }

    /**
     * Tests that resizing the Core properly delegates the call to the canvas.
     */
    @Test
    public void testResize() {
        Core core = new Core(WIDTH, HEIGHT);
        core.canvas = mock(Canvas.class);
        int newWidth = WIDTH * 2;
        int newHeight = HEIGHT * 2;

        core.resize(newWidth, newHeight);
        verify(core.canvas, times(1)).resize(newWidth, newHeight);
        verifyNoMoreInteractions(core.canvas);
    }

    /**
     * Tests that pressing F when not in fullscreen triggers fullscreen mode.
     */
    @Test
    public void testRenderFullscreenMode() {
        // Setup LibGDX mocks with F key pressed and a dummy display mode.
        DisplayMode dummyDisplayMode = mock(DisplayMode.class);
        Graphics mockGraphics = setupGdxMocks(true);
        when(mockGraphics.getDisplayMode()).thenReturn(dummyDisplayMode);

        Core core = new Core(WIDTH, HEIGHT);
        core.fullscreen = false; // Ensure initial state is windowed.

        core.render(); // Should trigger fullscreen mode.

        verify(mockGraphics, times(1)).setFullscreenMode(dummyDisplayMode);
        assertTrue(core.fullscreen, "Fullscreen should be true after triggering fullscreen mode");
    }

    /**
     * Tests that pressing F when already in fullscreen triggers windowed mode.
     */
    @Test
    public void testRenderWindowedMode() {
        // Setup LibGDX mocks with F key pressed.
        Graphics mockGraphics = setupGdxMocks(true);

        Core core = new Core(WIDTH, HEIGHT);
        core.fullscreen = true; // Initially in fullscreen.

        core.render(); // Should toggle back to windowed mode.

        verify(mockGraphics, times(1)).setWindowedMode(Config.SCREEN_WIDTH, Config.SCREEN_HEIGHT);
        assertFalse(core.fullscreen, "Fullscreen should be false after toggling back to windowed mode");
    }

    /**
     * Tests that if F is not pressed, the render() method does not change the
     * display mode.
     */
    @Test
    public void testRenderNoKeyPress() {
        // Setup LibGDX mocks with F key not pressed.
        Graphics mockGraphics = setupGdxMocks(false);

        Core core = new Core(WIDTH, HEIGHT);
        boolean initialFullscreen = core.fullscreen;

        core.render(); // Should not change any mode.

        verify(mockGraphics, never()).setFullscreenMode(any(DisplayMode.class));
        verify(mockGraphics, never()).setWindowedMode(anyInt(), anyInt());
        assertEquals(initialFullscreen, core.fullscreen, "Fullscreen state should remain unchanged");
    }

    /**
     * Tests that multiple consecutive render() calls with F pressed toggle the
     * display mode correctly.
     * <p>
     * This test simulates rapid toggling by forcing the key press on each call.
     */
    @Test
    public void testRenderMultipleToggles() {
        // Setup LibGDX mocks with F key pressed.
        DisplayMode dummyDisplayMode = mock(DisplayMode.class);
        Graphics mockGraphics = setupGdxMocks(true);
        when(mockGraphics.getDisplayMode()).thenReturn(dummyDisplayMode);

        Core core = new Core(WIDTH, HEIGHT);
        core.fullscreen = false; // Start in windowed mode.

        // First render: windowed -> fullscreen.
        core.render();
        assertTrue(core.fullscreen, "After first toggle, fullscreen should be true");
        verify(mockGraphics, times(1)).setFullscreenMode(dummyDisplayMode);

        // Simulate key press for next toggle.
        core.fullscreen = true;
        core.render();
        assertFalse(core.fullscreen, "After second toggle, fullscreen should be false");
        verify(mockGraphics, times(1)).setWindowedMode(Config.SCREEN_WIDTH, Config.SCREEN_HEIGHT);

        // Third toggle: windowed -> fullscreen again.
        core.fullscreen = false;
        core.render();
        assertTrue(core.fullscreen, "After third toggle, fullscreen should be true");
        verify(mockGraphics, times(2)).setFullscreenMode(dummyDisplayMode);
    }

    /**
     * Helper method to set up LibGDX mocks for Input and Graphics.
     *
     * @param isFKeyPressed Whether the F key should be simulated as pressed.
     * @return The mocked Graphics instance.
     */
    private Graphics setupGdxMocks(boolean isFKeyPressed) {
        Input mockInput = mock(Input.class);
        Graphics mockGraphics = mock(Graphics.class);
        Gdx.input = mockInput;
        Gdx.graphics = mockGraphics;
        when(mockInput.isKeyJustPressed(Input.Keys.F)).thenReturn(isFKeyPressed);
        return mockGraphics;
    }
}
