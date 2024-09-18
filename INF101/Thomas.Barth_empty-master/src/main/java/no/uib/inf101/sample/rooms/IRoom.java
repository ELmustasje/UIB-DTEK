package no.uib.inf101.sample.rooms;

import no.uib.inf101.sample.equipment.IEquipment;
import no.uib.inf101.sample.sprites.ISprite;

import java.awt.*;

/**
 * The IRoom interface defines the contract for rooms within the game.
 * Implementing classes are expected to provide functionality for drawing the room,
 * setting default values, managing sprites, updating room state, and handling room navigation.
 */
public interface IRoom {
    /**
     * Draws the room and its contents on the provided Graphics2D context.
     *
     * @param g2 The Graphics2D object used for drawing operations.
     */
    void drawRoom(Graphics2D g2);

    /**
     * Sets the default values or state for the room. This method is typically called when initializing
     * the room or resetting it to its initial state.
     */
    void setDefaultValues();

    /**
     * Sets the sprite (player or character) to the room, allowing the room to interact with the sprite.
     *
     * @param sprite The sprite to be added to the room.
     */
    void setSpriteToRoom(ISprite sprite);

    /**
     * Updates the state of the room, including any interactive elements or game logic.
     */
    void update();

    /**
     * Retrieves the location of the button to navigate to the next room.
     *
     * @return A Point object representing the location of the next room button.
     */
    Point getNextRoomButton();

    /**
     * Retrieves the location of the button to navigate to the previous room.
     *
     * @return A Point object representing the location of the previous room button.
     */
    Point getPrevRoomButton();

    /**
     * Retrieves the equipment plot associated with the room. The plot is an interactive element
     * where the player can use or interact with equipment.
     *
     * @return An IEquipment object representing the equipment plot in the room.
     */
    IEquipment getPlot();
}