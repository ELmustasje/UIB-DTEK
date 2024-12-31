package labyrinth.view;

import grid.GridCell;
import grid.GridDimension;
import labyrinth.model.GameState;

public interface ViewableTetrisModel {

    /**
     * Get dimensions of Wordle board
     * 
     * @return dimensions of board
     */
    public GridDimension getDimension();

    /**
     * Get tiles of labyrinth board
     * 
     * @return tiles of board
     */
    public Iterable<GridCell<Character>> getTilesOnBoard();

    /**
     * Get player on board
     * 
     * @return player on board
     */
    public Iterable<GridCell<Character>> getPlayer();

    /**
     * Get monster on board
     * 
     * @return monster on board
     */
    public Iterable<GridCell<Character>> getMonster();

    /**
     * Get the current score of the game
     * 
     * @return score of the game
     */
    public int getScore();

    /**
     * Get the current state of the game
     * 
     * @return current game state
     */
    public GameState getGameState();

}
