package inf112.utils;

import com.badlogic.gdx.Gdx;
import com.badlogic.gdx.audio.Music;
import com.badlogic.gdx.audio.Sound;

import inf112.core.Config;
import inf112.core.events.EventQueue;

import java.util.HashMap;

public class AudioManager {

    private static AudioManager instance;
    private float musicVolume = 0.6f;
    private float sfxVolume = 0.6f;
    private boolean musicMuted = false;
    private boolean sfxMuted = false;
    private final HashMap<String, Music> musicTracks;
    private final HashMap<String, Sound> sfxTracks;
    private Music currentMusic;
    private String currentMusicName;
    private final EventQueue queue;

    private AudioManager() {
        sfxTracks = new HashMap<>();
        musicTracks = new HashMap<>();
        queue = new EventQueue();
    }

    public static AudioManager getInstance() {
        if (instance == null) {
            instance = new AudioManager();
        }
        return instance;
    }

    public void loadSfx(String name, String filePath) {
        if (sfxTracks.containsKey(name))
            return;
        sfxTracks.put(name, Gdx.audio.newSound(Gdx.files.internal(filePath)));
    }

    public void loadSfx(String name, Sound sound) {
        if (sfxTracks.containsKey(name))
            return;
        sfxTracks.put(name, sound);
    }

    public void loadMusic(String name, String filePath) {
        if (musicTracks.containsKey(name))
            return;
        musicTracks.put(name, Gdx.audio.newMusic(Gdx.files.internal(filePath)));
    }

    public void loadMusic(String name, Music music) {
        if (musicTracks.containsKey(name))
            return;
        musicTracks.put(name, music);
    }

    public boolean musicLoaded(String name) {
        return musicTracks.containsKey(name);
    }

    public boolean sfxLoaded(String name) {
        return sfxTracks.containsKey(name);
    }

    public String getCurrentMusicName() {
        return currentMusicName;
    }

    public void pauseCurrentMusic() {
        if (currentMusic == null)
            throw new RuntimeException("Current music not loaded");
        currentMusic.pause();
    }

    public void resumeCurrentMusic() {
        if (currentMusic == null)
            throw new RuntimeException("Current music not loaded");
        currentMusic.play();
    }

    public void playMusic(String name, boolean loop) {
        if (Config.TEST)
            return;
        if (!musicTracks.containsKey(name))
            throw new RuntimeException("music" + name + " not loaded");
        if (currentMusic != null)
            currentMusic.stop();
        currentMusicName = name;
        currentMusic = musicTracks.get(name);
        currentMusic.setLooping(loop);
        currentMusic.setVolume(musicVolume);
        currentMusic.play();
    }

    public void playSfx(String name) {
        if (Config.TEST)
            return;
        if (!sfxTracks.containsKey(name))
            throw new RuntimeException("sfx" + name + " not loaded");
        if (!sfxMuted)
            sfxTracks.get(name).play(sfxVolume);
    }

    /**
     * To use the internal queue for await you must call this function every frame.
     */
    public void update() {
        queue.update();
    }

    public void stopMusic() {
        if (currentMusic != null && currentMusic.isPlaying()) {
            currentMusic.stop();
        }
    }

    public void setSfxVolume(float sfxVolume) {
        this.sfxVolume = sfxVolume;
    }

    public float getSfxVolume() {
        return this.sfxVolume;
    }

    public void setMusicVolume(float musicVolume) {
        this.musicVolume = musicVolume;
        if (currentMusic != null && !musicMuted) {
            currentMusic.setVolume(musicVolume);
        }
    }

    public float getMusicVolume() {
        return this.musicVolume;
    }

    public void muteMusic() {
        musicMuted = true;
        if (currentMusic != null) {
            currentMusic.setVolume(0);
        }
    }

    public void unmuteMusic() {
        musicMuted = false;
        if (currentMusic != null) {
            currentMusic.setVolume(musicVolume);
            if (!currentMusic.isPlaying()) {
                currentMusic.play();
            }
        }
    }

    public void toggleMusic() {
        if (musicMuted)
            unmuteMusic();
        else
            muteMusic();
    }

    public String musicStatus() {
        if (musicMuted) {
            return "Off";
        }
        return "On ";
    }

    public void muteSfx() {
        if (!sfxMuted)
            sfxMuted = true;
    }

    public void unmuteSfx() {
        if (sfxMuted)
            sfxMuted = false;
    }

    public void toggleSfx() {
        if (sfxMuted)
            unmuteSfx();
        else
            muteSfx();
    }

    public String sfxStatus() {
        if (sfxMuted) {
            return "Off";
        }
        return "On ";
    }

    public void removeMusic(String name) {
        if (!musicTracks.containsKey(name))
            throw new RuntimeException("Music " + name + " not loaded");
        Music music = musicTracks.get(name);
        if (music.isPlaying())
            music.stop();
        music.dispose(); // Properly dispose of the music
        musicTracks.remove(name);
    }

    public void removeSound(String name) {
        if (!sfxTracks.containsKey(name))
            throw new RuntimeException("SFX " + name + " not loaded");
        sfxTracks.get(name).dispose(); // Dispose sound before removing
        sfxTracks.remove(name);
    }

    public void clearMusic() {
        if (currentMusic != null && currentMusic.isPlaying()) {
            currentMusic.stop();
        }
        for (Music music : musicTracks.values()) {
            music.dispose();
        }
        musicTracks.clear();
        currentMusic = null; // Ensure we don't reference a disposed object
        currentMusicName = null;
    }

    public void clearSfx() {
        for (Sound sfx : sfxTracks.values()) {
            sfx.dispose();
        }
        sfxTracks.clear();
    }

    public void clearAll() {
        clearMusic();
        clearSfx();
    }

}
