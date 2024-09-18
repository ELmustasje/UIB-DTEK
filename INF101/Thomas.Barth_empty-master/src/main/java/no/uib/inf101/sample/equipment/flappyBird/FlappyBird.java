package no.uib.inf101.sample.equipment.flappyBird;

import no.uib.inf101.sample.GamePanel;
import no.uib.inf101.sample.MouseHandler;

import javax.imageio.ImageIO;
import java.awt.*;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

/**
 * The FlappyBird class encapsulates the game logic, rendering, and state management for the Flappy Bird mini-game.
 */
public class FlappyBird {
    private ArrayList<Obsticle> queue; // Queue to manage the obstacles

    private Bird bird; // The player-controlled bird
    private final BufferedImage backgroundImg;
    private final BufferedImage STARTtxt;
    private final BufferedImage quitButton;
    private final BufferedImage quitButtonGlow; // Images for the game's background and UI elements

    private final MouseHandler mouseHandler; // The MouseHandler for managing mouse interactions
    private boolean running, done; // Flags to track the game's running state and completion

    private int gameX, gameY, gameWidth, gameHeigth, score; // Game area dimensions and score
    private double speed; // Speed of the obstacles

    /**
     * Constructs a FlappyBird game instance with a reference to the MouseHandler.
     * It initializes the game's state and loads necessary images.
     *
     * @param mouseHandler The MouseHandler instance for managing mouse events related to FlappyBird.
     */
    public FlappyBird(MouseHandler mouseHandler) {
        this.mouseHandler = mouseHandler;
        reset();
        try {
            backgroundImg = ImageIO.read(new File("res/flappyBird/flappyBirdBackground.png"));
            STARTtxt = ImageIO.read(new File("res/Other/Starttxt.png"));
            quitButton = ImageIO.read(new File("res/Other/quitbutton.png"));
            quitButtonGlow = ImageIO.read(new File("res/Other/quitbuttonglow.png"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * Gets the current score of the game.
     *
     * @return The current score.
     */
    protected int getScore() {
        return score;
    }

    /**
     * Checks if the game is currently running.
     *
     * @return true if the game is running, false otherwise.
     */
    protected boolean isRunning() {
        return running;
    }

    /**
     * Gets the speed of the obstacles in the game.
     *
     * @return The speed of the obstacles.
     */
    protected double getSpeed() {
        return speed;
    }

    /**
     * Gets the height of the game area.
     *
     * @return The height of the game area.
     */
    protected int getGameHeigth() {
        return gameHeigth;
    }

    /**
     * Gets the y-coordinate of the game area's position.
     *
     * @return The y-coordinate of the game area.
     */
    protected int getGameY() {
        return gameY;
    }

    /**
     * Resets the game to its initial state, reinitializing the bird, obstacles, and score.
     */
    protected void reset() {
        running = false;
        done = false;
        score = 0;
        speed = 2;
        bird = new Bird();
        Obsticle obs1 = new Obsticle(670, this);
        Obsticle obs2 = new Obsticle(870, this);
        Obsticle obs3 = new Obsticle(1070, this);
        // Obstacles in the game
        Obsticle obs4 = new Obsticle(1270, this);
        queue = new ArrayList<>();
        queue.add(obs1);
        queue.add(obs2);
        queue.add(obs3);
        queue.add(obs4);
        gameX = 150;
        gameY = 151;
        gameHeigth = 214;
        gameWidth = 530;
    }

    /**
     * Starts the game by setting the running flag to true and the done flag to false.
     * This method should be called when the game is initialized or restarted.
     */
    protected void run() {
        done = false;
        running = true;
    }

    /**
     * Checks if the game has been completed or exited.
     *
     * @return true if the game is done, false otherwise.
     */
    protected boolean done() {
        return done;
    }

    /**
     * Draws the game and its elements on the provided Graphics2D context.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */

    protected void draw(Graphics2D g2) {
        drawGame(g2);
        g2.drawImage(backgroundImg, 0, 0, GamePanel.WIDTH, GamePanel.HEIGHT, null);
    }


private void drawGame(Graphics2D g2) {
    // Draw the game area
    g2.setColor(Color.green);
    g2.fillRect(gameX, gameY, gameWidth, gameHeigth);

    // Draw the bird and obstacles
    bird.draw(g2);
    for (Obsticle obs : queue) {
        obs.draw(g2);
    }

    // Draw the score
    g2.setFont(new Font("_", Font.PLAIN, 30));
    g2.setColor(Color.RED);
    g2.drawString(String.valueOf(score), gameX + 25, gameY + 35);

    // Draw the start and quit buttons when the game is not running
    if (!running) {
        g2.setColor(Color.CYAN);
        g2.fillRect(gameX + gameWidth / 3, gameY + gameHeigth / 3, gameWidth / 3, gameHeigth / 3);
        g2.drawImage(STARTtxt, gameX + gameWidth / 3, gameY + gameHeigth / 3, gameWidth / 3, gameHeigth / 3, null);
        if (!hoveringExitButton()) {
            g2.drawImage(quitButton, gameX + gameWidth - 42, gameY + 5, 25, 25, null);
        } else {
            g2.drawImage(quitButtonGlow, gameX + gameWidth - 42, gameY + 5, 25, 25, null);
        }
    }
}


    private boolean mouseInsideGame() {
        return (
                mouseHandler.mouseX > gameX &&
                        mouseHandler.mouseX < gameX + gameWidth &&
                        mouseHandler.mouseY > gameY &&
                        mouseHandler.mouseY < gameY + gameHeigth
        );
    }


    private boolean hoveringStartButton() {
        return (
                mouseHandler.mouseX > gameX + gameWidth / 3 &&
                        mouseHandler.mouseX < gameX + ((gameWidth / 3) * 2) &&
                        mouseHandler.mouseY > gameY + gameHeigth / 3 &&
                        mouseHandler.mouseY < gameY + ((gameHeigth / 3) * 2)
        );
    }


    private boolean hoveringExitButton() {
        return (
                mouseHandler.mouseX > gameX + gameWidth - 42 &&
                        mouseHandler.mouseX < gameX + gameWidth - 42 + 25 &&
                        mouseHandler.mouseY > gameY + 5 &&
                        mouseHandler.mouseY < gameY + 30
        );
    }

    /**
     * Updates the game state, including the bird, obstacles, and checks for collisions and game over conditions.
     */
    protected void update() {
        // Game loop efficiency and logic adapted from a StackExchange post by Zerro97, retrieved on 24.04.24

        final int FPS = 60;
        final long OptimalTime = 1000000000 / FPS;

        if (running) {
            long now;
            long updateTime;
            long wait;

            now = System.nanoTime();

            // Update the bird and obstacles
            bird.update();
            for (Obsticle obs : queue) {
                obs.update();
            }

            // Check for collisions or if the bird is out of bounds
            if (bird.birdCrash(queue.get(0)) || bird.outOfBounds(this)) {
                running = false;
            } else {
                // Update the score and speed as the bird passes obstacles
                if (queue.get(0).x < bird.getX()) {
                    score++;
                    if (score % 3 == 0 && score > 0) {
                        speed += 0.2;
                    }
                    Obsticle shuffle = queue.remove(0);
                    queue.add(shuffle);
                }
            }

            // Check if the player has initiated a jump
            if (mouseInsideGame()) {
                if (mouseHandler.mousePressed) {
                    bird.jump();
                }
            }

            // Calculate the time taken to update the game
            updateTime = System.nanoTime() - now;
            wait = (OptimalTime - updateTime) / 1000000;

            // Delay to maintain a consistent frame rate
            if (wait >= 0) {
                try {
                    Thread.sleep(wait);
                } catch (Exception e) {
                    System.out.println("Thread can't sleep");
                }
            }
        } else {
            // Check if the start button is pressed to begin the game
            if (hoveringStartButton() && mouseHandler.mousePressed) {
                reset();
                run();
            } else if (hoveringExitButton() && mouseHandler.mousePressed) {
                // Check if the exit button is pressed to end the game
                done = true;
                mouseHandler.used();
            }
        }
    }
}