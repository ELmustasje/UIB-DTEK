package labyrinth;

import java.awt.Color;

import datastructure.GridDirection;
import datastructure.CellPosition;

public interface ILabyrinth {

	/**
	 * Get labyrinth cell contents at a coordinate
	 * 
	 * @param pos the position
	 * @return The tile at the position
	 * @throws IllegalArgumentException unless 0 <= x < {@link #numberOfColumns()}
	 *                                  and 0 <= y <
	 *                                  {@link #numberOfRows()}
	 */
	LabyrinthTile getCell(CellPosition pos);

	/**
	 * Get the color of the cell in a given position.
	 * 
	 * @param pos the coordinate
	 * @return The color of the cell in the given row and column.
	 */
	Color getColor(CellPosition pos);

	/**
	 * @return The number of rows.
	 */
	int numberOfRows();

	CellPosition getPlayerPos();

	/**
	 * @return Current amount of player gold
	 */
	int getPlayerGold();

	/**
	 * @return Current number of player hit points
	 */
	int getPlayerHitPoints();

	/**
	 * @return The number of columns.
	 */
	int numberOfColumns();

	/**
	 * @return True if the game is active
	 */
	boolean isPlaying();

	/**
	 * Move player one step in direction dir
	 * If player can not move in that direction leave the player in same position.
	 * 
	 * @param dir The direction the player should move in
	 * @throws MovePlayerException <player making an illegal move>
	 */
	void movePlayer(GridDirection dir) throws MovePlayerException;

	double distToGold(CellPosition pos);

	void aiMove();
	/**
	 * Check if a move is valid.
	 * A move is considered invalid if there is a wall in the new position
	 * or if the move brings the player outside the bounds of the grid.
	 * 
	 * @param d The direction the play should move in
	 * @return True if movePlayer(d) is a valid move
	 */
	boolean playerCanGo(GridDirection d);

	/**
	 * Iterable over all coordinates in this CellAutomaton
	 */
	Iterable<CellPosition> coordinates();

}
