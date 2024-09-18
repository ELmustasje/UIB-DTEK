package no.uib.inf101.sample.midi;

import javax.sound.midi.MidiSystem;
import javax.sound.midi.Sequencer;
import java.io.*;

/**
 * The ChadSong class implements the Runnable interface and is responsible for playing a MIDI song.
 * It manages the loading of the song file and controls the playback using a Sequencer.
 */
public class ChadSong implements Runnable {
    private final InputStream song;

    /**
     * Constructor for ChadSong. It initializes the InputStream for the MIDI song file.
     * If the file is not found, it throws a RuntimeException.
     */
    public ChadSong(){
        try {
            song = new FileInputStream("res/Other/chadSong.mid");
        } catch (FileNotFoundException e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public void run() {
        try {
            Sequencer sequencer;
            (sequencer = MidiSystem.getSequencer()).setSequence(MidiSystem.getSequence(song));
            sequencer.open();
            sequencer.setLoopCount(-1);
            sequencer.start();
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

}