package cellular;

import cellular.cellstate.ICellState;
import datastructure.CellPosition;

/**
 * An CellAutomaton represents a Cellular Automaton. The
 * automaton contains a cell generation organized in rows
 * and columns.
 * 
 * @author Anna Eilertsen - anna.eilertsen@uib.no
 */
public interface ICellAutomaton {

	/**
	 * Get the state of the cell in the provided row and column
	 * 
	 * @param pos    The position of the cell
	 * @return The state of the cell in the given row and column.
	 */
	ICellState getCellState(CellPosition pos);

	/**
	 * Sets the start-state for each cell
	 */
	void initializeCells();

	/**
	 * Updates the state of the cell according to the rules of the automaton
	 */
	void step();

	/**
	 * @return The number of rows in this automaton
	 */
	int numberOfRows();

	/**
	 * @return The number of columns in this automaton
	 */
	int numberOfColumns();

	/**
	 * Iterable over all Locations in this CellAutomaton
	 */
	Iterable<CellPosition> locations();
}
