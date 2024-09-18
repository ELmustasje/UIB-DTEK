package no.uib.inf101.tetris.controller;

import no.uib.inf101.tetris.model.GameState;

public interface ControllableTetrisModel {

    /**
     * Moves the current tetromino by the specified amount of rows and columns.
     *
     * @param deltaRow  the number of rows to move the tetromino
     * @param deltaCol  the number of columns to move the tetromino
     * @return true if the tetromino was successfully moved, false otherwise
     */
    boolean moveTetromino(int deltaRow, int deltaCol);

    /**
     * Rotates the current tetromino.
     *
     * @return true if the tetromino was successfully rotated, false otherwise
     */
    boolean rotateTetromino();

    /**
     * Drops the current tetromino to the bottom of the grid.
     */
    void dropTetromino();

    /**
     * Returns the current game state.
     *
     * @return the current game state
     */
    GameState getGameState();

    /**
     * Returns the number of milliseconds between each clock tick.
     *
     * @return the number of milliseconds between each clock tick
     */
    int getMillis();

    /**
     * Advances the game by one clock tick.
     */
    void clockTick();
}
