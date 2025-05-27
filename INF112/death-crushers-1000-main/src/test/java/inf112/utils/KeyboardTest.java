package inf112.utils;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Input;
import com.badlogic.gdx.InputAdapter;
import com.badlogic.gdx.InputProcessor;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

public class KeyboardTest {

    @BeforeEach
    public void setUp() {
        // Mock Gdx.input
        Gdx.input = mock(Input.class);
    }

    @Test
	public void testKeyDown() {
		when(Gdx.input.isKeyPressed(Input.Keys.W)).thenReturn(true);
		assertTrue(Keyboard.down(Input.Keys.W), "Expected W key to be pressed.");
	}

    @Test
	public void testKeyNotDown() {
		when(Gdx.input.isKeyPressed(Input.Keys.W)).thenReturn(false);
		assertFalse(Keyboard.down(Input.Keys.W), "Expected W key to NOT be pressed.");
	}

    @Test
	public void testKeyJustPressed() {
		when(Gdx.input.isKeyJustPressed(Input.Keys.SPACE)).thenReturn(true);
		assertTrue(Keyboard.pressed(Input.Keys.SPACE), "Expected SPACE key to be just pressed.");
	}

    @Test
	public void testKeyNotJustPressed() {
		when(Gdx.input.isKeyJustPressed(Input.Keys.SPACE)).thenReturn(false);
		assertFalse(Keyboard.pressed(Input.Keys.SPACE), "Expected SPACE key to NOT be just pressed.");
	}

    @Test
	public void testAnyKeyDown() {
		when(Gdx.input.isKeyPressed(Input.Keys.ANY_KEY)).thenReturn(true);
		assertTrue(Keyboard.anyKeyDown(), "Expected any key to be down.");
	}

    @Test
	public void testAnyKeyPressed() {
		when(Gdx.input.isKeyJustPressed(Input.Keys.ANY_KEY)).thenReturn(true);
		assertTrue(Keyboard.anyKeyPressed(), "Expected any key to be just pressed.");
	}

    @Test
    public void testKeyTyped() {
        // Simulating key input using a custom InputProcessor
        InputProcessor mockProcessor = new InputAdapter() {
            @Override
            public boolean keyTyped(char character) {
                assertEquals('a', character, "Expected 'a' to be typed.");
                return true;
            }
        };

        Gdx.input.setInputProcessor(mockProcessor);
        mockProcessor.keyTyped('a');
    }
}
