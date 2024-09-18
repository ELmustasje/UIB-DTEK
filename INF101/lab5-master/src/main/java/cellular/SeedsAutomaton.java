package cellular;

import java.util.Random;

import cellular.cellstate.CellState;
import cellular.cellstate.ICellState;
import datastructure.Grid;
import datastructure.GridDirection;
import datastructure.IGrid;
import datastructure.CellPosition;

/**
 * An ICellAutomata that implements the Seeds Cellular Automaton.
 * 
 * @author eivind
 * @author Martin Vatshelle - martin.vatshelle@uib.no
 * @see ICellAutomaton
 *      <p>
 *      Every cell has two states: Alive or Dead. Each step the state of each
 *      cell is decided from its neighbors (diagonal, horizontal and lateral).
 *      If a dead cell has exactly two alive neighbors then it becomes alive,
 *      otherwise it dies.
 */
public class SeedsAutomaton implements ICellAutomaton {

	/**
	 * The grid containing the current generation.
	 */
	IGrid<ICellState> currentGeneration;

	/**
	 * Construct a Seeds ICellAutomaton using a grid with the given height and
	 * width.
	 * 
	 * @param rows number of rows
	 * @param columns number of columns
	 */
	public SeedsAutomaton(int rows, int columns) {
		currentGeneration = new Grid<>(rows, columns,
				CellState.DEAD);
	}

	@Override
	public void initializeCells() {
		Random random = new Random();
		for (CellPosition loc : currentGeneration.cellPositions()) {
			if (random.nextBoolean()) {
				currentGeneration.set(loc, CellState.ALIVE);
			} else {
				currentGeneration.set(loc, CellState.DEAD);
			}
		}
	}

	@Override
	public int numberOfRows() {
		return currentGeneration.rows();
	}

	@Override
	public int numberOfColumns() {
		return currentGeneration.cols();
	}

	@Override
	public ICellState getCellState(CellPosition pos) {
		return currentGeneration.get(pos);
	}

	@Override
	public void step() {

		IGrid<ICellState> nextGeneration = new Grid<>(
				currentGeneration.rows(), currentGeneration.cols(),
				CellState.ALIVE);

		for (CellPosition loc : currentGeneration.cellPositions()) {
			int numNeighbours = countNeighbours(loc, CellState.ALIVE);
			if (numNeighbours == 2) {
				nextGeneration.set(loc, CellState.ALIVE);
			} else {
				nextGeneration.set(loc, CellState.DEAD);
			}
		}

		currentGeneration = nextGeneration;
	}

	private int countNeighbours(CellPosition loc, CellState state) {
		int numNeighbors = 0;
		for (GridDirection dir : GridDirection.values()) {
			CellPosition neighbor = loc.getNeighbor(dir);

			if (currentGeneration.isOnGrid(neighbor)) {
				try {
					if (currentGeneration.get(neighbor) == state) {
						numNeighbors++;
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
		return numNeighbors;
	}

	@Override
	public Iterable<CellPosition> locations() {
		return currentGeneration.cellPositions();
	}
}
