package inf112.utils;

import com.badlogic.gdx.audio.Music;
import com.badlogic.gdx.audio.Sound;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

class AudioManagerTest {
    private AudioManager audioManager;
    private Sound mockSound;
    private Music mockMusic;

    @BeforeEach
    public void setUp() {
        // Create mock instances
        mockSound = mock(Sound.class);
        mockMusic = mock(Music.class);
        audioManager = AudioManager.getInstance();
        audioManager.clearAll();
    }

    @Test
    public void testSingleInstance() {
        AudioManager audioManager2 = AudioManager.getInstance();
        assertEquals(audioManager, audioManager2);
    }

    @Test
    public void testLoadMusic() {
        audioManager.loadMusic("test", mockMusic);
        assertTrue(audioManager.musicLoaded("test"));
        assertFalse(audioManager.musicLoaded("foo"));
    }

    @Test
    public void testLoadSfx() {
        audioManager.loadSfx("test", mockSound);
        assertTrue(audioManager.sfxLoaded("test"));
        assertFalse(audioManager.sfxLoaded("foo"));
    }

    @Test
    public void testGetCurrentMusicName() {
        audioManager.loadMusic("test", mockMusic);
        audioManager.playMusic("test", false);
        assertEquals("test", audioManager.getCurrentMusicName());
    }

    /*
     * Basic Usage of verify()
     * Checking if a Method Was Called
     * 
     * verify(mockObject).someMethod();
     * 
     * This checks if someMethod() was called at least once on mockObject. If not,
     * the test fails.
     */

    @Test
    public void testPlayMusic() {
        audioManager.loadMusic("test", mockMusic);
        audioManager.playMusic("test", false);
        verify(mockMusic, times(1)).play();
    }

    @Test
    public void testStopMusic() {
        audioManager.loadMusic("test", mockMusic);
        audioManager.playMusic("test", false);
        Mockito.when(mockMusic.isPlaying()).thenReturn(true);
        audioManager.stopMusic();
        verify(mockMusic, times(1)).stop();
    }

    @Test
    public void testPauseMusic() {
        audioManager.loadMusic("test", mockMusic);
        audioManager.playMusic("test", false);
        audioManager.pauseCurrentMusic();
        verify(mockMusic, times(1)).pause();
    }

    @Test
    public void testResumeMusic() {
        audioManager.loadMusic("test", mockMusic);
        audioManager.playMusic("test", false);
        audioManager.pauseCurrentMusic();
        audioManager.resumeCurrentMusic();
        verify(mockMusic, times(2)).play(); // Once for play, once for resume
    }

    @Test
    public void testMuteAndUnmuteMusic() {
        audioManager.loadMusic("test", mockMusic);
        audioManager.playMusic("test", false);
        audioManager.muteMusic();
        verify(mockMusic, times(1)).setVolume(0.0f);

        audioManager.unmuteMusic();
        verify(mockMusic, times(2)).setVolume(1.0f);
    }

    @Test
    public void testMuteAndUnmuteSfx() {
        audioManager.loadSfx("test", mockSound);
        audioManager.muteSfx();
        audioManager.playSfx("test");
        verify(mockSound, never()).play(anyFloat());

        audioManager.unmuteSfx();
        audioManager.playSfx("test");
        verify(mockSound, times(1)).play(1.0f);
    }

    @Test
    public void testRemoveMusic() {
        audioManager.loadMusic("test", mockMusic);
        assertTrue(audioManager.musicLoaded("test"));
        audioManager.removeMusic("test");
        assertFalse(audioManager.musicLoaded("test"));
    }

    @Test
    public void testRemoveSfx() {
        audioManager.loadSfx("test", mockSound);
        assertTrue(audioManager.sfxLoaded("test"));
        audioManager.removeSound("test");
        assertFalse(audioManager.sfxLoaded("test"));
    }

    @Test
    public void testClearMusic() {
        audioManager.loadMusic("test", mockMusic);
        assertTrue(audioManager.musicLoaded("test"));
        audioManager.clearMusic();
        assertFalse(audioManager.musicLoaded("test"));
    }

    @Test
    public void testClearSfx() {
        audioManager.loadSfx("test", mockSound);
        assertTrue(audioManager.sfxLoaded("test"));
        audioManager.clearSfx();
        assertFalse(audioManager.sfxLoaded("test"));
    }

    @Test
    public void testClearAll() {
        audioManager.loadMusic("test", mockMusic);
        audioManager.loadSfx("test", mockSound);
        assertTrue(audioManager.musicLoaded("test"));
        assertTrue(audioManager.sfxLoaded("test"));

        audioManager.clearAll();
        assertFalse(audioManager.musicLoaded("test"));
        assertFalse(audioManager.sfxLoaded("test"));
    }

    @Test
    void setSfxVolume() {
        audioManager.setSfxVolume(1.0f);
        assertEquals(1.0f, audioManager.getSfxVolume(), 0.0f);
    }

    @Test
    void getSfxVolume() {
        assertEquals(1.0f, audioManager.getSfxVolume(), 0.0f);
    }

    @Test
    void setMusicVolume() {
        audioManager.setMusicVolume(1.0f);
        assertEquals(1.0f, audioManager.getMusicVolume(), 0.0f);
    }

    @Test
    void getMusicVolume() {
        assertEquals(1.0f, audioManager.getMusicVolume(), 0.0f);
    }

    @Test
    void toggleMusic() {
        String status = audioManager.musicStatus();
        audioManager.toggleMusic();
        assertNotEquals(status, audioManager.musicStatus());

    }

    @Test
    void musicStatus() {
        assertTrue(audioManager.musicStatus().equals("On ") || audioManager.musicStatus().equals("Off"));
    }

    @Test
    void sfxStatus() {
        assertTrue(audioManager.sfxStatus().equals("On ") || audioManager.sfxStatus().equals("Off"));
    }

    @Test
    void toggleSfx() {
        String status = audioManager.sfxStatus();
        audioManager.toggleSfx();
        assertNotEquals(status, audioManager.sfxStatus());
    }

}
