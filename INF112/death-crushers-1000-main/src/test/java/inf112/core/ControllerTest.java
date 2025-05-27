package inf112.core;

import inf112.gfx.Interactable;
import inf112.utils.Mouse;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.util.List;

import static org.mockito.Mockito.*;

class ControllerTest {

    private Controller controller = new Controller();

    @Test
    void testUpdateHoverClick() {
        Interactable interactable = mock(Interactable.class);

        try (MockedStatic<Mouse> mouse = mockStatic(Mouse.class)) {
            mouse.when(() -> Mouse.over(interactable)).thenReturn(true);
            mouse.when(Mouse::clicked).thenReturn(true);

            controller.update(List.of(interactable));

            verify(interactable).hover(true);
            verify(interactable).click(true);
        }
    }
}
