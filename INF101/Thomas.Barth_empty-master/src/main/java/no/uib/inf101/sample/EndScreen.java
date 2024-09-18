package no.uib.inf101.sample;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

/**
 * The {@code EndScreen} class represents the end screen of a game.
 * It displays a message to the player and shows two images that change over time.
 */
public class EndScreen {
    private BufferedImage sprite1;
    private final BufferedImage sprite2;
    private int sprite2Size = 0;

    /**
     * Constructs an {@code EndScreen} with a specified {@code MouseHandler}.
     *
    */

      public EndScreen() {
        try {
            sprite1 = ImageIO.read(new File("res/sprites/spriteDefault.png"));
            sprite2 = ImageIO.read(new File("res/sprites/gigachad.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Updates the state of the end screen, including the size of the second sprite.
     * This method should be called in the game loop to animate the end screen.
     */
    void update() {
        // Game loop efficiency and logic adapted from a StackExchange post by Zerro97, retrieved on 24.04.24
        //https://gamedev.stackexchange.com/questions/160329/java-game-loop-efficiency
        final long FPS = 10;
        final long OptimalTime = 1000000000 / FPS;

        long now;
        long updateTime;
        long wait;

        now = System.nanoTime();
        sprite2Size++;
        if (sprite2Size > 250) {
            sprite1 = null;
            if (sprite2Size > 350) {
                sprite2Size = 350;
            }
        }
        updateTime = System.nanoTime() - now;
        wait = (OptimalTime - updateTime) / 1000000;

        if (wait >= 0) {
            try {
                Thread.sleep(wait);
            } catch (Exception e) {
                System.out.println("Thread can't sleep");
            }
        }
    }

    /**
     * Draws the end screen on the provided {@code Graphics2D} object.
     * This method should be called in the game's render loop.
     *
     * @param g2 The {@code Graphics2D} object on which to draw the end screen.
     */
    public void draw(Graphics2D g2) {
        g2.setColor(Color.black);
        g2.fillRect(0, 0, GamePanel.WIDTH, GamePanel.HEIGHT);
        g2.setColor(Color.white);
        g2.setFont(new Font("_", Font.PLAIN, 50));
        String text1 = "Hey YOU!!";
        g2.drawString(text1, 10, 50);
        String text2 = "YOU HAVE NOW BECOME";
        g2.drawString(text2, 10, 100);
        String text3 = "THE ULITMATE DOUCHEBAG";
        g2.drawString(text3, 10, 150);

        if (sprite1 != null) {
            g2.drawImage(sprite1, (GamePanel.WIDTH / 2) - 150 / 2, (GamePanel.HEIGHT / 2) - 400 / 2 + 100, 150, 400, null);
        }
        g2.drawImage(sprite2, (GamePanel.WIDTH / 2) - sprite2Size / 2, (GamePanel.HEIGHT / 2) - (sprite2Size + 50) / 2 + 100, sprite2Size, sprite2Size + 50, null);
    }

}