package no.uib.inf101.tetris.model;

import no.uib.inf101.grid.CellPosition;
import no.uib.inf101.grid.Grid;
import no.uib.inf101.grid.GridCell;

import java.util.Iterator;
import java.util.NoSuchElementException;

public class TetrisBoard extends Grid<Character> {

    /**
     * Constructs a Tetris board with the specified number of rows and columns.
     *
     * @param rows The number of rows in the board.
     * @param cols The number of columns in the board.
     */
    public TetrisBoard(int rows, int cols) {
        super(rows, cols, '-');
    }

    /**
     * Returns a string representation of the Tetris board in a pretty format.
     *
     * @return A string representation of the Tetris board.
     */
    public String prettyString() {
        Iterator<GridCell<Object>> it = this.iterator();
        StringBuilder ps = new StringBuilder();
        int counter = 0;
        while (it.hasNext()) {
            ps.append(it.next().value());
            if (counter == this.cols() - 1) {
                ps.append("\n");
                counter = 0;
            } else {
                counter++;
            }
        }
        return ps.substring(0, ps.length() - 1);
    }

    /**
     * Removes any full rows in the Tetris board and shifts down the rows above.
     *
     * @return The number of rows that were removed.
     */
    public int removeFullRows() {
        int removedRows = 0;
        Iterator<GridCell<Object>> it = fullgrid.iterator();
        int fullCount = 0;
        while (it.hasNext()) {
            GridCell<Object> cell = it.next();
            if (cell.value().equals(getDefaultValue())) {
                for (int i = 0; i < cols() - fullCount - 1; i++) {
                    try {
                        it.next();
                    } catch (NoSuchElementException e) {
                        break;
                    }
                }
                fullCount = 0;
            } else {
                fullCount++;
                if (cell.pos().col() == this.cols() - 1) {
                    if (fullCount == cols()) {
                        clearRow(cell.pos().row());
                        shiftDownFrom(cell.pos().row());
                        removedRows++;
                    }
                    fullCount = 0;
                }
            }
        }
        return removedRows;
    }

    /**
     * Clears a specified row in the Tetris board by setting all cells to the default value.
     *
     * @param row The row to be cleared.
     */
    private void clearRow(int row) {
        for (int i = 0; i < cols(); i++) {
            set(new CellPosition(row, i), getDefaultValue());
        }
    }

    /**
     * Shifts down the rows above a specified row in the Tetris board.
     *
     * @param fromRow The starting row to shift down from.
     */
    private void shiftDownFrom(int fromRow) {
        for (int i = fromRow - 1; i >= 0; i--) {
            for (int j = 0; j < cols(); j++) {
                Object value = get(new CellPosition(i, j)).value();
                if (!value.equals(getDefaultValue())) {
                    set(new CellPosition(i, j), getDefaultValue());
                    set(new CellPosition(i + 1, j), value);
                }
            }
        }
    }
}