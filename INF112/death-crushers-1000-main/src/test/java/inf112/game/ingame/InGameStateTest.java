package inf112.game.ingame;

import inf112.core.Config;
import inf112.core.state.StateManager;
import inf112.utils.AudioManager;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class InGameStateTest {

    private InGameState inGameState;
    private StateManager parentManager;
    private StateManager mockStateManager;
    private static MockedStatic<AudioManager> mockedAudioManager;

    @BeforeEach
    void setUp() {
        Config.TEST = true;
        parentManager = mock(StateManager.class);
        mockStateManager = mock(StateManager.class);

        // Mock static AudioManager once before all tests
        mockedAudioManager = Mockito.mockStatic(AudioManager.class);
        AudioManager mockAudioInstance = mock(AudioManager.class);
        mockedAudioManager.when(AudioManager::getInstance).thenReturn(mockAudioInstance);

        inGameState = new InGameState(1);
        inGameState.init(parentManager);

        // Inject the mocked StateManager
        inGameState.state = mockStateManager;
    }

    @AfterEach
    void tearDown() {
        // Close static mock to prevent conflicts
        mockedAudioManager.close();
    }

    @Test
    void testInit_initializesDependenciesCorrectly() {
        assertNotNull(inGameState.deck());
        assertNotNull(inGameState.map());
        assertNotNull(inGameState.player());

        mockedAudioManager.verify(AudioManager::getInstance, times(1)); // Ensure AudioManager is accessed
    }

    @Test
    void testUpdate_callsStateUpdate() {
        inGameState.update();
        verify(mockStateManager, times(1)).update();
    }

    @Test
    void testClean_callsStateClean() {
        inGameState.clean();
        verify(mockStateManager, times(1)).clean();
    }
}
