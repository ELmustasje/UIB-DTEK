package labyrinth.controller;

import grid.GridDirection;
import labyrinth.model.GameState;

public interface ControllableLabyrinthModel {

    /**
     * Move player in given direction
     * @param dir
     */
    void movePlayer(GridDirection dir);

    /**
     * Get the current state of the game
     * 
     * @return current game state
     */
    GameState getGameState();

    /**
     * Get delay time
     * @return time delay
     */
    int getTimerDelay();

    /**
     * Method to execute certain functionality for each time tick
     */
    void clockTick();

    /**
     * Reset the game back to start
     */
    void reset();

}
