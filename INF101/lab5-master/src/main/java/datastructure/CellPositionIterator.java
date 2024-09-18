package datastructure;

import java.util.Iterator;

/**
 * This class is able to iterate over all positions in a grid.
 * 
 * @author Martin Vatshelle - martin.vatshelle@uib.no
 */
public class CellPositionIterator implements Iterator<CellPosition> {

	int numRows;
	int numCols;
	CellPosition current;

	/**
	 * Constructs a CellPositionIterator
	 *
	 * @param numRows - number of rows in the grid
	 * @param numCols - number of columns in the grid
	 */
	public CellPositionIterator(int numRows, int numCols) {
		this.numRows = numRows;
		this.numCols = numCols;
		current = new CellPosition(0, 0);
	}

	/**
	 * Constructs a CellPositionIterator
	 * 
	 * @param gd - the dimensions we should iterate through
	 */
	public CellPositionIterator(GridDimension gd) {
		this(gd.rows(), gd.cols());
	}


	@Override
	public boolean hasNext() {
		return current.row() < numRows && current.col() < numCols;
	}

	@Override
	public CellPosition next() {
		CellPosition elem = current;
		if (current.col() < numCols - 1) {
			current = current.getNeighbor(GridDirection.EAST);
		} else {
			current = new CellPosition(current.row() + 1, 0);
		}
		return elem;
	}
}
