package no.uib.inf101.tetris.view;

import no.uib.inf101.grid.GridCell;
import no.uib.inf101.grid.GridDimension;
import no.uib.inf101.tetris.model.GameState;

public interface ViewableTetrisModel {

    /**
     * Returns the number of rows in the game board.
     *
     * @return The number of rows in the game board.
     */
    int rows();

    /**
     * Returns the number of columns in the game board.
     *
     * @return The number of columns in the game board.
     */
    int cols();

    /**
     * Returns the dimension of the game board.
     *
     * @return The dimension of the game board.
     */
    GridDimension getDimention();

    /**
     * Returns an iterable collection of GridCells representing the tiles on the game board.
     *
     * @return An iterable collection of GridCells representing the tiles on the game board.
     */
    Iterable<GridCell<Character>> getTilesOnBoard();

    /**
     * Returns an iterable collection of GridCells representing the falling tetromino.
     *
     * @return An iterable collection of GridCells representing the falling tetromino.
     */
    Iterable<GridCell<Character>> fallingTetromino();

    /**
     * Returns the current game state.
     *
     * @return The current game state.
     */
    GameState getGameState();
}

