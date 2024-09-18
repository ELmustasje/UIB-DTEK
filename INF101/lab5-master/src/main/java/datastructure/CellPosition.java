package datastructure;

/**
 * This class represents a position on a grid.
 * That means indices for row and column.
 * <p>
 * CellPosition is Immutable, this means the only way to make a new
 * CellPosition is to call the constructor.
 */
public record CellPosition(int row, int col) {

	/**
	 * This method is just for convenience.
	 * 
	 * @see GridDirection#getNeighbor(CellPosition)
	 * @param dir direction in which to get neighbouring CellPosition
	 * @return a neighbour CellPosition
	 */
	public CellPosition getNeighbor(GridDirection dir) {
		return dir.getNeighbor(this);
	}
}
