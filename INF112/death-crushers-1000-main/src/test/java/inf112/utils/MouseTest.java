package inf112.utils;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.Graphics;
import com.badlogic.gdx.Input;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

public class MouseTest {

    private static MockedStatic<Gdx> gdxMock;
    private Input inputMock;
    private Graphics graphicsMock;

    @BeforeAll
    static void init() {
        gdxMock = mockStatic(Gdx.class, Mockito.CALLS_REAL_METHODS);
    }

    @AfterAll
    static void tearDown() {
        gdxMock.close(); // Ensure Gdx mock is released after all tests
    }

    @BeforeEach
    void setUp() {
        inputMock = mock(Input.class);
        graphicsMock = mock(Graphics.class);
        Gdx.input = inputMock;
        Gdx.graphics = graphicsMock;
    }

    @Test
	void testMouseDown() {
		when(Gdx.input.isButtonPressed(Input.Buttons.LEFT)).thenReturn(true);
		assertTrue(Mouse.down());
		when(Gdx.input.isButtonPressed(Input.Buttons.LEFT)).thenReturn(false);
		assertFalse(Mouse.down());
	}

    @Test
	void testMouseClicked() {
		when(Gdx.input.isButtonJustPressed(Input.Buttons.LEFT)).thenReturn(true);
		assertTrue(Mouse.clicked());
		when(Gdx.input.isButtonJustPressed(Input.Buttons.LEFT)).thenReturn(false);
		assertFalse(Mouse.clicked());
	}

    @Test
	void testMouseRealPos() {
		when(Gdx.input.getX()).thenReturn(300);
		when(Gdx.input.getY()).thenReturn(250);

		Vec2 realPos = Mouse.realPos();
		assertEquals(300, realPos.x);
		assertEquals(250, realPos.y);
	}

}
