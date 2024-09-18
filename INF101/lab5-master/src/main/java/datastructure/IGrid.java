package datastructure;

import cellular.cellstate.ICellState;

import java.util.List;

/**
 * ICellStateGrid is a grid of ICellStates
 * 
 * @author Anna Eilertsen - anna.eilertsen@uib.no
 */
public interface IGrid<T> extends GridDimension {

	/**
	 * Set the contents of the cell in the given position.
	 * <p>
	 * row must be greater than or equal to 0 and less than rows().
	 * col must be greater than or equal to 0 and less than cols().
	 *
	 * @param pos     The position of the cell to change the contents of.
	 * @param element The contents the cell is to have.
	 */
	void set(CellPosition pos, T element);

	List<T> getCells();

	/**
	 * Get the contents of the cell in the given position.
	 * <p>
	 * row must be greater than or equal to 0 and less than rows().
	 * col must be greater than or equal to 0 and less than cols().
	 *
	 * @param pos the position of the cell to get the contents of.
	 */
	T get(CellPosition pos);

	/**
	 * Make a copy
	 * 
	 * @return A shallow copy of the grid, with the same elements
	 */
	IGrid copy();
}
