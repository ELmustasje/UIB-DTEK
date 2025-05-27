package inf112.core.state;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import java.util.List;

import inf112.gfx.Drawable;
import inf112.gfx.Interactable;
import inf112.utils.Timer;

class StateManagerTest {

    private StateManager manager;
    private IState initialState;
    private IState newState;

    @BeforeEach
    void setUp() {
        initialState = Mockito.mock(IState.class);
        newState = Mockito.mock(IState.class);
        manager = new StateManager(initialState);
    }

    @Test
    void testInitialStateInitCalled() {
        Mockito.verify(initialState).init(manager);
    }

    @Test
    void testSwitchTo() {
        manager.switchTo(newState);
        Mockito.verify(initialState).clean();
        Mockito.verify(newState).init(manager);
    }

    @Test
    void testAwaitSwitch() {
        Timer mockTimer = Mockito.mock(Timer.class);
        manager = new StateManager(initialState);
        manager.timer = mockTimer;

        manager.awaitSwitch(newState, 2.0f);

        Mockito.verify(mockTimer).start(2.0f);
    }

    @Test
    void testDrawablesAndInteractables() {
        List<Drawable> mockDrawables = List.of(Mockito.mock(Drawable.class));
        List<Interactable> mockInteractables = List.of(Mockito.mock(Interactable.class));

        Mockito.when(initialState.drawables()).thenReturn(mockDrawables);
        Mockito.when(initialState.interactables()).thenReturn(mockInteractables);

        assertEquals(mockDrawables, manager.drawables());
        assertEquals(mockInteractables, manager.interactables());
    }

    @Test
    void testCleanCallsStateClean() {
        manager.clean();
        Mockito.verify(initialState).clean();
        manager.clear();
        Mockito.verify(initialState).clear();
    }
}
