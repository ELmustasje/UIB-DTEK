package no.uib.inf101.sample.sprites;

import java.awt.*;

/**
 * The ISprite interface defines the contract for sprite objects within the game.
 * Implementing classes are expected to provide functionality for drawing the sprite,
 * updating its state, managing its position and size, and handling sprite-specific properties.
 */
public interface ISprite {
    /**
     * Draws the sprite on the provided Graphics2D context.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */
    void drawSprite(Graphics2D g2);



    /**
     * Sets the default values or state for the sprite. This method is typically called when initializing
     * the sprite or resetting it to its initial state.
     */
    void setDefaultValues();

    /**
     * Sets the position and size of the sprite.
     *
     * @param x      The x-coordinate of the sprite's position.
     * @param y      The y-coordinate of the sprite's position.
     * @param width  The width of the sprite.
     * @param height The height of the sprite.
     */
    void setValues(int x, int y, int width, int height);

    /**
     * Gets the width of the sprite.
     *
     * @return The width of the sprite.
     */
    int getWidth();

    /**
     * Gets the height of the sprite.
     *
     * @return The height of the sprite.
     */
    int getHeight();

    /**
     * Gets the strength attribute of the sprite.
     *
     * @return The strength of the sprite.
     */
    int getStrength();

    /**
     * Gets the x-coordinate of the sprite's position.
     *
     * @return The x-coordinate of the sprite.
     */
    int getX();

    /**
     * Gets the y-coordinate of the sprite's position.
     *
     * @return The y-coordinate of the sprite.
     */
    int getY();

    /**
     * Sets the sprite's mirrored state, which determines the direction the sprite is facing.
     *
     * @param turn true to mirror the sprite (face the opposite direction), false otherwise.
     */
    void mirrorTurn(boolean turn);

    /**
     * Updates the strength attribute of the sprite by a specified delta value.
     *
     * @param deltaStrength The amount by which to increase or decrease the sprite's strength.
     */
    void updateStrength(int deltaStrength);

    /**
     * Stops the drawing of the sprite, typically used to temporarily hide the sprite.
     */
    void stopDrawing();

    /**
     * Starts the drawing of the sprite, typically used to make the sprite visible after being hidden.
     */
    void startDrawing();

    /**
     * Checks if the sprite is currently mirrored.
     *
     * @return true if the sprite is mirrored, false otherwise.
     */
    boolean isMirrored();

    /**
     * Draws the sprite's stats, such as health or strength, on the provided Graphics2D context.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */
    void drawStats(Graphics2D g2);
}
