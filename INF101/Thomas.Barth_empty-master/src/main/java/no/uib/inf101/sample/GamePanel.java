package no.uib.inf101.sample;

import no.uib.inf101.sample.midi.ChadSong;
import no.uib.inf101.sample.rooms.RoomManager;
import no.uib.inf101.sample.sprites.Sprite;
import javax.swing.*;
import java.awt.*;

/**
 * The GamePanel class is responsible for managing the game's main loop and rendering.
 * It extends JPanel and implements Runnable for use in a separate thread.
 */
public class GamePanel extends JPanel implements Runnable {
    // SCREEN SETTINGS
    /**
     * The width of the game panel.
     */
    public final static int WIDTH = 800;
    /**
     * The height of the game panel.
     */
    public final static int HEIGHT = 600;

    MouseHandler mouseH = new MouseHandler();
    Thread gameThread;
    Sprite sprite = new Sprite(this);
    RoomManager roomManager = new RoomManager(sprite, mouseH);
    boolean victory = false;
    EndScreen endScreen= new EndScreen();

    /**
     * Constructor for the GamePanel class. It sets up the panel size, double buffering,
     * mouse listener, and starts the background music.
     */
    public GamePanel() {
        this.setPreferredSize(new Dimension(WIDTH, HEIGHT));
        this.setDoubleBuffered(true);
        this.addMouseListener(mouseH);
        this.setFocusable(true);
        ChadSong chadSong = new ChadSong();
        chadSong.run();
    }

    /**
     * Starts the game thread and the game loop.
     */
    public void startGameThread() {
        gameThread = new Thread(this);
        gameThread.start();
    }

    @Override
    public void run() {
        while (gameThread != null) {
            update();
            repaint();
        }
    }

    private void update() {
        if(victory){
            endScreen.update();
            return;
        }
        if (mouseH.mousePressed) {
            roomManager.update();
            mouseH.update();
            mouseH.used();
        } else {
            roomManager.update();
            mouseH.update();
        }
        if(sprite.getStrength() == 10){
            victory = true;
        }
    }





    @Override
    public void paintComponent(Graphics g) {
        super.paintComponent(g);
        Graphics2D g2 = (Graphics2D) g;
        if(victory){
            endScreen.draw(g2);
            return;
        }

        roomManager.draw(g2);
        sprite.drawSprite(g2);
        g2.dispose();

    }
}