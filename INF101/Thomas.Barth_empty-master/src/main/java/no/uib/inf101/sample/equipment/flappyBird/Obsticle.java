package no.uib.inf101.sample.equipment.flappyBird;

import java.awt.*;
import java.util.Random;

/**
 * The Obsticle class represents an obstacle (pipe) in the Flappy Bird mini-game.
 * It handles the obstacle's position, movement, and rendering.
 */
public class Obsticle {
    protected int x; // The x-coordinate of the obstacle's position
    protected int y; // The y-coordinate of the obstacle's position
    protected int height; // The height of the obstacle
    protected int width; // The width of the obstacle
    protected int opening = 70; // The vertical opening between the top and bottom parts of the obstacle
    private final Random random; // Random number generator for obstacle height
    private final FlappyBird game; // Reference to the FlappyBird game instance

    /**
     * Constructs an Obsticle object with a specified x-coordinate and a reference to the FlappyBird game.
     * It initializes the obstacle's position, size, and loads the images for the top and bottom parts.
     *
     * @param x The initial x-coordinate of the obstacle.
     * @param game The FlappyBird game instance that this obstacle is part of.
     */
    public Obsticle(int x, FlappyBird game) {
        this.game = game;
        random = new Random();
        this.x = x;
        y = 150; // Default y-coordinate for the top part of the obstacle
        width = 25; // Default width of the obstacle
        height = random.nextInt(20, 120); // Random height for the top part of the obstacle
    }

    /**
     * Updates the obstacle's position based on the game's speed.
     * Resets the obstacle's position and height when it moves off-screen.
     */
    protected void update() {
        x -= game.getSpeed();
        if (x < 100) {
            x = 870; // Reset x-coordinate to the right side of the screen
            height = random.nextInt(20, 120); // Generate a new random height for the top part
        }
    }

    /**
     * Draws the obstacle on the provided Graphics2D context.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */
    protected void draw(Graphics2D g2) {
        g2.setColor(Color.black);
        g2.fillRect(x,y,width,height);
        g2.fillRect(x,y+opening+height,width,200);
    }
}