package datastructure;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import cellular.cellstate.CellState;

import java.util.Random;
import org.junit.jupiter.api.Test;

public class GridTest {

	Random random = new Random(0L);
	Grid grid;

	@Test
	public void checkRowAndColumnEqualToConstructionParametersGiven() {
		int expectedRows = 11;
		int expectedColumns = 17;
		setGrid(expectedRows, expectedColumns);

		assertEquals(expectedRows, grid.rows());
		assertEquals(expectedColumns, grid.cols());
	}

	private void setGrid(int rows, int columns) {
		grid = new Grid(rows, columns, CellState.DEAD);
	}

	/**
	 * Tests that trying to access outside of the dimensions of the grid throws
	 * an IndexOutOfBoundsException.
	 */
	@Test
	public void outOfBoundsTest() {

		int expectedRows = 11;
		int expectedColumns = 17;

		setGrid(expectedRows, expectedColumns);

		assertDoesNotThrow(() -> grid.checkCoordinate(new CellPosition(0, 0)),
				"Throw exception when given row is equal to or greater than expectedRows: " + expectedRows);

		assertDoesNotThrow(() -> grid.checkCoordinate(new CellPosition(expectedRows - 1, expectedColumns - 1)),
				"Throw exception when given row is equal to or greater than expectedRows: " + expectedRows);

		assertThrows(IndexOutOfBoundsException.class, () -> grid.checkCoordinate(new CellPosition(expectedRows, 0)),
				"Throw exception when given row is equal to or greater than expectedRows: " + expectedRows);

		assertThrows(IndexOutOfBoundsException.class, () -> grid.checkCoordinate(new CellPosition(0, expectedColumns)),
				"Throw exception when given column is equal to or greater than expectedColumns: " + expectedColumns);

		assertThrows(IndexOutOfBoundsException.class, () -> grid.checkCoordinate(new CellPosition(-1, 0)),
				"Throw exception when given row is negative");

		assertThrows(IndexOutOfBoundsException.class, () -> grid.checkCoordinate(new CellPosition(0, -1)),
				"Throw exception when given column is negative");
	}

	@Test
	public void checkStateSetIsEqualBeforeSettingAnyOtherCell() {
		setGrid(100, 101);

		for (CellPosition pos : grid.cellPositions()) {
			CellState cs = CellState.random(random);
			grid.set(pos, cs);
			assertEquals(cs, grid.get(pos), "Failed to update cell (row " + pos.row() + ", col " + pos.col() + ")");
		}
	}

	@Test
	public void checkStateCanBeSetMultipleTimes() {
		setGrid(101, 100);

		for (CellPosition pos : grid.cellPositions()) {
			grid.set(pos, CellState.random(random));
		}

		for (CellPosition pos : grid.cellPositions()) {
			CellState cs = CellState.random(random);
			grid.set(pos, cs);
			assertEquals(cs, grid.get(pos), "Failed to update cell (row " + pos.row() + ", col " + pos.col() + ")");
		}
	}

	@Test
	public void copyTest() {
		setGrid(100, 105);

		for (CellPosition pos : grid.cellPositions()) {
			grid.set(pos, CellState.random(random));
		}

		IGrid newGrid = grid.copy();
		for (CellPosition pos : grid.cellPositions()) {
			assertEquals(grid.get(pos), newGrid.get(pos),
					"Cell at (row " + pos.row() + ", col " + pos.col() + ") is not properly copied over to the new grid");
		}
	}
}
