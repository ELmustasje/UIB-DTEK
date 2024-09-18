package no.uib.inf101.sample.equipment.flappyBird;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

/**
 * The Bird class represents the player-controlled bird in the Flappy Bird mini-game.
 * It handles the bird's physics, rendering, and collision detection.
 */
public class Bird {
    private double speedY = 0;
    private final int x;
    private int y;
    private final int width;
    private final int height;

    private final BufferedImage birdImg;

    /**
     * Constructs a Bird object and initializes its position, size, and image.
     */
    public Bird() {
        x = 200;
        y = 250;
        width = 25;
        height = 25;
        try {
            birdImg = ImageIO.read(new File("res/flappyBird/flappybird.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Gets the x-coordinate of the bird's position.
     *
     * @return The x-coordinate of the bird.
     */
    protected int getX() {
        return x;
    }

    /**
     * Draws the bird on the provided Graphics2D context.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */
    protected void draw(Graphics2D g2) {
        g2.drawImage(birdImg, x, y, width, height, null);
    }

    /**
     * Makes the bird jump by setting its vertical speed to a negative value, simulating a jump.
     */
    protected void jump() {
        double jumpPower = 3;
        speedY = -jumpPower;
    }

    /**
     * Updates the bird's position based on its vertical speed and gravity.
     */
    protected void update() {
        double gravity = 0.2;
        speedY += gravity;
        y += speedY;
    }

    /**
     * Checks if the bird has collided with an obstacle.
     *
     * @param obs The obstacle to check for collision with the bird.
     * @return true if the bird has collided with the obstacle, false otherwise.
     */
    protected boolean birdCrash(Obsticle obs) {
        if ((x > obs.x && x < obs.x + obs.width)
                || (x + width > obs.x && x + width < obs.x + obs.width)) {
            return !(y > obs.y + obs.height && y + height < obs.y + obs.opening + obs.height);
        }
        return false;
    }

    /**
     * Checks if the bird is out of the game bounds.
     *
     * @param game The FlappyBird game instance to check the bounds against.
     * @return true if the bird is out of bounds, false otherwise.
     */
    protected boolean outOfBounds(FlappyBird game) {
        return !(y > game.getGameY() - 10
                && y + height < game.getGameY() + game.getGameHeigth() + 10);
    }
}