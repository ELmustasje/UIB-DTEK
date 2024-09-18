package no.uib.inf101.sample.equipment;

import no.uib.inf101.sample.rooms.Plot;
import no.uib.inf101.sample.sprites.ISprite;

import java.awt.*;

/**
 * The IEquipment interface defines the contract for equipment objects within the game.
 * Implementing classes are expected to provide functionality for using the equipment,
 * managing its state, handling interactions, and drawing the equipment and its stats.
 */
public interface IEquipment {
    /**
     * Triggers the use of the equipment, typically involving some game logic or interaction.
     */
    void use();

    /**
     * Checks if the equipment is currently in use.
     *
     * @return true if the equipment is in use, false otherwise.
     */
    boolean isInUse();

    /**
     * Determines if the use of the equipment was successful, often based on game conditions or player actions.
     *
     * @return true if the set was successful, false otherwise.
     */
    boolean successfulSet();

    /**
     * Resets the equipment to its default state, typically after use or when changing levels.
     */
    void resetEquipment();

    /**
     * Handles the logic for when the mouse is hovering over the equipment.
     */
    void hovering();

    /**
     * Sets the default image for the equipment, typically used for its idle or inactive state.
     */
    void setDefaultImg();

    /**
     * Gets the height of the equipment.
     *
     * @return The height of the equipment.
     */
    int getHeight();

    /**
     * Gets the interval at which the equipment can be skipped or interacted with again.
     *
     * @return The skip interval in milliseconds or game ticks.
     */
    int getSkipInterval();

    /**
     * Gets the width of the equipment.
     *
     * @return The width of the equipment.
     */
    int getWidth();

    /**
     * Gets the y-coordinate of the equipment's position.
     *
     * @return The y-coordinate of the equipment.
     */
    int getY();

    /**
     * Gets the x-coordinate of the equipment's position.
     *
     * @return The x-coordinate of the equipment.
     */
    int getX();

    /**
     * Adds the equipment to a given plot, associating it with a specific area in the room.
     *
     * @param plot The plot to which the equipment will be added.
     */
    void addToPlot(Plot plot);

    /**
     * Draws the equipment on the provided Graphics2D context.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */
    void draw(Graphics2D g2);

    /**
     * Draws the equipment's stats, such as usage or effectiveness, on the provided Graphics2D context.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */
    void drawStats(Graphics2D g2);

    /**
     * Updates the state of the equipment, typically called each frame or tick of the game loop.
     */
    void update();

    /**
     * Changes the difficulty of using the equipment based on the sprite's attributes or state.
     *
     * @param sprite The sprite interacting with the equipment.
     */
    void changeDifficulty(ISprite sprite);
}