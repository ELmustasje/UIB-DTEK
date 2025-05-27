package inf112.utils;

import inf112.gfx.Canvas;
import inf112.gfx.Palette;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class UITest {

    private Canvas mockCanvas;

    @BeforeEach
    public void setUp() {
        mockCanvas = mock(Canvas.class);
    }

    // Test cases for button()
    @Test
	public void testButtonClicked() {
		when(mockCanvas.getMouseWorldPosition()).thenReturn(new Vec2(50, 50)); // Inside the button

		// Mocking static method Mouse.down() to return true when the button is clicked
		try (MockedStatic<Mouse> mockedMouse = mockStatic(Mouse.class)) {
			mockedMouse.when(Mouse::clicked).thenReturn(true);

			boolean result = UI.button(mockCanvas, 30, 30, 100, 50, "Click Me");
			assertTrue(result); // The button should be clicked
		}
	}

    @Test
	public void testButtonNotClicked() {
		when(mockCanvas.getMouseWorldPosition()).thenReturn(new Vec2(200, 200)); // Outside the button

		// Mocking static method Mouse.down() to return true when the button is clicked
		try (MockedStatic<Mouse> mockedMouse = mockStatic(Mouse.class)) {
			mockedMouse.when(Mouse::down).thenReturn(true);

			boolean result = UI.button(mockCanvas, 30, 30, 100, 50, "Click Me");
			assertFalse(result); // The button should not be clicked
		}
	}

    // Test cases for slider()
    @Test
	public void testSliderInitialValue() {
		when(mockCanvas.getMouseWorldPosition()).thenReturn(new Vec2(150, 50)); // Inside slider handle

		// Mocking Mouse.down() method
		try (MockedStatic<Mouse> mockedMouse = mockStatic(Mouse.class)) {
			mockedMouse.when(Mouse::down).thenReturn(true);

			int result = UI.slider(mockCanvas, 30, 30, 0, 100, 1, "slider1", 50);
			assertEquals(50, result); // Initial value should be within range
		}
	}

    @Test
	public void testSliderDragging() {
		when(mockCanvas.getMouseWorldPosition()).thenReturn(new Vec2(150, 50)); // Inside the slider handle

		// Mocking Mouse.down() to simulate dragging
		try (MockedStatic<Mouse> mockedMouse = mockStatic(Mouse.class)) {
			mockedMouse.when(Mouse::down).thenReturn(true);

			int result = UI.slider(mockCanvas, 30, 30, 0, 100, 1, "slider1", 50);
			assertTrue(result >= 0 && result <= 100); // Should be within min-max range
		}
	}

    // Test cases for checkbox()
    @Test
	public void testCheckboxToggled() {
		when(mockCanvas.getMouseWorldPosition()).thenReturn(new Vec2(50, 50)); // Inside checkbox area

		// Mocking Mouse.clicked() to simulate a click
		try (MockedStatic<Mouse> mockedMouse = mockStatic(Mouse.class)) {
			mockedMouse.when(Mouse::clicked).thenReturn(true);

			boolean result = UI.checkbox(mockCanvas, 30, 30, 20, 20, "checkbox1");
			assertTrue(result); // Checkbox should toggle
		}
	}

    @Test
	public void testCheckboxNotToggled() {
		when(mockCanvas.getMouseWorldPosition()).thenReturn(new Vec2(100, 100)); // Outside checkbox area

		// Mocking Mouse.clicked() to simulate no click
		try (MockedStatic<Mouse> mockedMouse = mockStatic(Mouse.class)) {
			mockedMouse.when(Mouse::clicked).thenReturn(false);

			boolean result = UI.checkbox(mockCanvas, 30, 30, 20, 20, "checkbox1");
			assertFalse(result); // Checkbox should not toggle
		}
	}

    @Test
	public void testTextBoxInactive() {
		when(mockCanvas.getMouseWorldPosition()).thenReturn(new Vec2(100, 100)); // Outside the textbox
		try (MockedStatic<Mouse> mockedMouse = mockStatic(Mouse.class)) {
			mockedMouse.when(Mouse::clicked).thenReturn(false);

			String result = UI.textBox(mockCanvas, 30, 30, 200, 30, "textBox1");
			assertEquals("", result); // No input should be recorded
		}
	}

    // Additional tests

    @Test
	public void testButtonColorChangeOnHover() {
		when(mockCanvas.getMouseWorldPosition()).thenReturn(new Vec2(50, 50)); // Inside the button

		// Mocking Mouse.down() to simulate no click
		try (MockedStatic<Mouse> mockedMouse = mockStatic(Mouse.class)) {
			mockedMouse.when(Mouse::down).thenReturn(false);

			boolean result = UI.button(mockCanvas, 30, 30, 100, 50, "Hover Me", Palette.BUTTON_HOVER);
			assertFalse(result); // Button should not be clicked, only hovered
		}
	}

    @Test
	public void testSliderBoundsHandling() {
		// Test if slider respects min and max bounds properly
		when(mockCanvas.getMouseWorldPosition()).thenReturn(new Vec2(50, 50));
		try (MockedStatic<Mouse> mockedMouse = mockStatic(Mouse.class)) {
			mockedMouse.when(Mouse::down).thenReturn(true);

			int result = UI.slider(mockCanvas, 30, 30, 0, 10, 1, "slider1", 5);
			assertEquals(5, result); // Slider should start at value 5
		}
	}

}
