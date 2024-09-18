package datastructure;

import java.util.ArrayList;
import java.util.List;


/**
 * A Grid contains a set of cell states
 *
 * @author Martin Vatshelle - martin.vatshelle@uib.no
 */
public class Grid<T> implements IGrid<T> {
	private final List<T> cells;
	private final int columns;
	private final int rows;

	/**
	 * Construct a grid with the given dimensions.
	 * 
	 * @param rows the number of rows
	 * @param columns the number of cols
	 * @param initElement What the cells should initially hold (possibly null)
	 */
	public Grid(int rows, int columns, T initElement) {
		if (rows <= 0 || columns <= 0) {
			throw new IllegalArgumentException();
		}

		this.columns = columns;
		this.rows = rows;
		this.cells = new ArrayList<>(columns * rows);
		for (int i = 0; i < columns * rows; ++i) {
			this.cells.add(initElement);
		}
	}

	public List<T> getCells(){
		return cells;
	}
	@Override
	public int cols() {
		return columns;
	}

	@Override
	public int rows() {
		return rows;
	}

	@Override
	public void set(CellPosition pos, T elem) {
		this.checkCoordinate(pos);
		this.cells.set(coordinateToIndex(pos), elem);
	}

	/**
	 * This method checks if a given CellPosition is within the bounds of this grid.
	 * If it is not, an IndexOutOfBoundsException is thrown.
	 * 
	 * @param pos the position to check
	 */
	public void checkCoordinate(CellPosition pos) {
		if (!this.isOnGrid(pos)) {
			throw new IndexOutOfBoundsException("Row and column indices must be within bounds");
		}
	}

	/**
	 * This method computes which index in the list belongs to a given Coordinate
	 */
	private int coordinateToIndex(CellPosition pos) {
		return pos.row() + pos.col() * rows;
	}

	@Override
	public T get(CellPosition pos) {
		this.checkCoordinate(pos);
		return this.cells.get(coordinateToIndex(pos));
	}

	@Override
	public IGrid<T> copy() {
		Grid<T> newGrid = new Grid<>(rows(), cols(), null);
		for (CellPosition pos : this.cellPositions()) {
			newGrid.set(pos, this.get(pos));
		}
		return newGrid;
	}
}
